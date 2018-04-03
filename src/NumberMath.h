#ifndef NUMBER_MATH_H
#define NUMBER_MATH_H

#include <random>
using namespace std;

#include "Number.hpp"

namespace NumberMath
{
    Number random()
    {
        return Number(rand() % 10000007);
    }

    Number gcd(const Number& a1, const Number& b1)
    {
        Number a = Number::abs(a1), b = Number::abs(b1);
        while (true)
        {
            if (a.zero()) return b;
            b %= a;
            if (b.zero()) return a;
            a %= b;
        }
    }

    Number gcdExtended(const Number& a, const Number &b, Number& x, Number& y)
    {
        if (a == 0)
        {
            x = 0; y = 1;
            return b;
        }
        Number x1, y1;
        Number d = gcdExtended(b % a, a, x1, y1);
        x = y1 - (b / a) * x1;
        y = x1;
        return d;
    }

    Number lcm(const Number& a, const Number& b)
    {
        return (a * b / gcd(a, b)).set_neg(a.neg || b.neg);
    }

    Number powMod(Number a, Number b, const Number &modulo)
    {
        assert(b >= 0);
        Number x = 1;
        while (b > 0)
        {
            if (b % 2 == 1)
            {
                x = (x*a) % modulo;
            }
            a = (a*a) % modulo;
            b /= 2;
        }
        return x % modulo;
    }

    bool isPrimeMillerRabin(const Number &p, int iterations = 10)
    {
        if (p < 2) return false;
        if (p != 2 && p % 2 == 0) return false;

        Number s = p-1;
        while (s % 2 == 0) s /= 2;

        for (int i = 0; i < iterations; ++i)
        {
            Number a = random() % (p-1) + 1, tmp = s;
            Number mod = powMod(a, tmp, p);
            while (tmp != p-1 && mod != 1 && mod != p-1)
            {
                mod = (mod*mod) % p;
                tmp *= 2;
            }
            if (mod != p-1 && tmp % 2 == 0)
            {
                return false;
            }
        }
        return true;
    }

    Number factorRhoPollard(const Number &a)
    {
        if (a == 1) return 1;
        if (a % 2 == 0) return 2;

        Number factor = 1, x = 2, xFixed = x;
        int cycleSize = 2;

        while (factor == 1)
        {
            for (int i = 0; i < cycleSize && factor == 1; ++i)
            {
                x = (x*x + 1) % a;
                factor = gcd(x - xFixed, a);
            }

            cycleSize *= 2;
            xFixed = x;
        }

        if (!isPrimeMillerRabin(factor))
        {
            for (Number f = 2; f <= factor; ++f)
            {
                if (isPrimeMillerRabin(f) && factor % f == 0) return f;
            }
        }

        return factor;
    }

    vector<Number> factorizeRhoPollard(Number a)
    {
        assert(a >= 2);
        vector<Number> res;
        while (a > 1)
        {
            res.push_back(factorRhoPollard(a));
            a /= res.back();
        }
        return res;
    }

    int mobius(const Number& a)
    {
        assert(a >= 1);
        if (a == 1) return 1;
        auto f = factorizeRhoPollard(a);
        for (int i = 1; i < f.size(); ++i)
            if (f[i] == f[i-1]) return 0;
        return f.size() % 2 ? -1 : 1;
    }

    Number euler(Number a)
    {
        assert(a >= 1);
        if (a == 1) return 1;
        if (isPrimeMillerRabin(a)) return a - 1;
        auto f = factorizeRhoPollard(a);
        for (int i = 0; i < f.size(); ++i)
        {
            if (i > 0 && f[i] == f[i-1]) continue;
            a /= f[i];
            a *= f[i] - 1;
        }
        return a;
    }

    int legendre(const Number& a, const Number& p)
    {
        assert(p >= 3 && isPrimeMillerRabin(p));
        Number res = powMod(a, (p - 1) / 2, p);
        if (res == p-1) res = -1;
        return res.to_int();
    }

    int jacobi(const Number& a, const Number& n)
    {
        assert(n >= 1 && n % 2 == 1);

        if (a == 0)
            return (n == 1) ? 1 : 0;
        else if (a == 2)
        {
            switch ((n % 8).to_int())
            {
                case 1:
                case 7:
                    return 1;
                case 3:
                case 5:
                    return -1;
                default:break;
            }
        }
        else if (a >= n)
            return jacobi(a % n, n);
        else if (a % 2 == 0)
            return jacobi(2, n) * jacobi(a / 2, n);
        else
            return (a % 4 == 3 && n % 4 == 3) ? -jacobi(n, a) : jacobi(n, a);
    }

    Number inverse(const Number &a, const Number &mod)
    {
        return powMod(a, euler(mod) - 1, mod);
    }

    Number logarithmRhoPollard(const Number& a, const Number& b, const Number& p)
    {
        assert(a >= 0 && b >= 2 && p >= 0);
        Number u1 = 0, u2 = 0, v1 = 0, v2 = 0, z1 = 1, z2 = 1;
        Number pm1 = p - 1, pd3 = p / 3, pd32 = pd3 * 2;
        auto f = [&](Number& u, Number& v, Number& z)
        {
            if (z < pd3)
            {
                ++u;
                u = u % pm1;
                z = (b * z) % p;
            }
            else if (pd32 < z)
            {
                ++v;
                v  = v % pm1;
                z = (a * z) % p;
            }
            else {
                u = (u * 2) % pm1;
                v  = (v * 2) % pm1;
                z = (z * z) % p;
            }
        };

        f(u2, v2, z2);
        do {
            f(u1, v1, z1);

            f(u2, v2, z2);
            f(u2, v2, z2);
        } while (z1 != z2);

        Number du = u1 - u2, dv = v2 - v1, d = gcd(du, pm1);
        if (d == 1)
        {
            return ((dv % pm1) * inverse(du % pm1, pm1)) % pm1;
        }

        Number pm1dd = pm1 / d, bmodp = b % p;
        Number l = ((dv % pm1dd) * inverse(du % pm1dd, pm1dd)) % (pm1dd);
        if (l < 0) l += pm1dd;
        for (Number m = 0; m <= d; ++m)
        {
            if (powMod(a, l, p) == bmodp)
                return l;
            l = (l + pm1dd) % pm1;
        }

        return 0;
    }

    pair< pair<Number, Number>, pair<Number, Number> > generateKeysRSA()
    {
        Number p(random()), q(random());

        while (!isPrimeMillerRabin(p)) ++p;
        while (!isPrimeMillerRabin(q)) ++q;

        Number n = p * q;
        Number phi = (p-1) * (q-1);

        Number e = Number(random()) % (phi-3) + 3;
        while (gcd(phi, e) != 1) e = Number(random()) % (phi-3) + 3;
        Number d = inverse(e, phi);

        return { {e, n}, {d, n} };
    }

    vector<Number> encryptRSA(const std::string& message, const pair<Number, Number>& publicKey)
    {
        vector<Number> cipher;
        for (char c : message)
            cipher.push_back(powMod(Number(c), publicKey.first, publicKey.second));
        return cipher;
    }

    std::string decryptRSA(const vector<Number>& cipher, pair<Number, Number>& privateKey)
    {
        std::string message;
        for (const auto &c : cipher)
            message += (char) powMod(c, privateKey.first, privateKey.second).to_int();
        return message;
    }

    Number montgomeryMul(Number a, Number b, const Number& mod)
    {
        assert((mod[0] & 1) && mod >= 3);

        Number reducer = 2;
        while (reducer <= mod)
        {
            reducer *= 2;
        }
        assert(gcd(reducer, mod) == 1);

        Number inverse = NumberMath::inverse(reducer, mod);
        Number factor = (reducer * inverse - 1) / mod;

        a = (a * reducer) % mod;
        b = (b * reducer) % mod;

        Number x = a * b;
        Number s = (x * factor) % reducer;
        Number t = x + s * mod;
        Number u = t / reducer;
        Number c = (u < mod) ? u : u - mod;
        return (c * inverse) % mod;
    }
}

#endif

#ifndef NUMBER_H
#define NUMBER_H

#include <cmath>
#include <cstdint>
#include <climits>
#include <string>

#include <vector>
#include <algorithm>
#include <cassert>

class Number {
    typedef unsigned short type;

private:
    std::vector<type> digits;
public:
    bool neg;

    static const type base = 10;

    Number() : neg(false) {}

    Number(size_t n, type w, bool neg = false) : digits(n, w), neg(neg) {}

    Number(int i) : neg(i < 0)
    {
        auto u = (unsigned int)std::abs(i);
        while (u > 0)
        {
            push_back((type)(u % base));
            u /= base;
        }
    }

    Number(const char *c) : neg(false)
    {
        if (*c == '-')
        {
            c++;
            neg = true;
        }
        for (; *c; c++)
        {
            mul(base);
            add(type(*c - '0'));
        }
    }

    Number(const Number& a)
    {
        digits = a.digits;
        neg = a.neg;
    }

    Number& operator= (const Number& a)
    {
        digits = a.digits;
        neg = a.neg;
        return *this;
    }

    static int cmp_abs(const Number& a, const Number& b)
    {
        auto na = (int)a.size(), nb = (int)b.size();
        if (na != nb) return na < nb ? -1 : 1;

        for (int i = na-1; i >= 0; --i)
        {
            type wa = a[i], wb = b[i];
            if (wa != wb) return wa < wb ? -1 : 1;
        }
        return 0;
    }

    static int cmp(const Number& a, const Number& b)
    {
        if (a.zero() && b.zero()) return 0;
        if (!a.neg && !b.neg) return cmp_abs(a, b);
        if ( a.neg &&  b.neg) return -cmp_abs(a, b);
        return a.neg && !b.neg ? -1 : 1;
    }

    static Number mul(const Number& a, const Number& b)
    {
        size_t na = a.size(), nb = b.size(), nc = na + nb;

        Number c(nc, 0, a.neg ^ b.neg), carries(nc, 0);

        for (size_t ia = 0; ia < na; ia++)
        {
            for (size_t ib = 0; ib < nb; ib++)
            {
                size_t i = ia + ib;
                type tmp = a[ia] * b[ib];
                carries += (Number(add_carry(&c[i], tmp % base) + tmp / base) << i+1);
            }
        }
        add_unsigned(c, carries);
        c.truncate();
        return c;
    }

    static Number div(const Number& numerator, const Number& denominator)
    {
        Number quotient, remainder;
        div_mod(numerator, denominator, quotient, remainder);
        return quotient;
    }

    static Number mod(const Number& numerator, const Number& denominator)
    {
        Number quotient, remainder;
        div_mod(numerator, denominator, quotient, remainder);
        return remainder;
    }

    static Number add(const Number& a, const Number& b)
    {
        Number result = add_signed(a, a.neg, b, b.neg);
        return result;
    }

    static Number sub(const Number& a, const Number& b)
    {
        Number result = add_signed(a, a.neg, b, !b.neg);
        return result;
    }

    static Number abs(Number a)
    {
        a.set_neg(false);
        return a;
    }

    int sgn() const
    {
        return neg ? -1 : 1;
    }

    std::string to_string() const
    {
        if (zero()) return "0";

        std::string res;
        for (size_t i = 0; i < size(); ++i) res.push_back('0' + (char)digits[i]);
        if (neg) res.push_back('-');
        std::reverse(res.begin(), res.end());

        return res;
    }

    int to_int() const
    {
        if (zero()) return 0;

        int res = 0;
        for (int i = (int)size()-1; i >= 0; --i)
        {
            res *= 10;
            res += digits[i];
        }

        return sgn() * res;
    }

    Number& set_neg(bool neg)
    {
        this->neg = neg;
        return *this;
    }

    type& operator [] (size_t i) { return digits[i]; }
    const type& operator [] (size_t i) const { return digits[i]; }

    friend std::istream& operator >> (std::istream& stream, Number& r)
    {
        std::string s;
        stream >> s;
        r = Number(s.c_str());
        return stream;
    }

    friend std::ostream& operator << (std::ostream& stream, const Number& r)
    {
        return stream << r.to_string();
    }

    Number& operator ++ () { add(1); return *this; }

    Number& operator += (const Number& b) { return *this = add(*this, b); }
    Number& operator -= (const Number& b) { return *this = sub(*this, b); }
    Number& operator *= (const Number& b) { return *this = mul(*this, b); }
    Number& operator /= (const Number& b) { return *this = div(*this, b); }
    Number& operator %= (const Number& b) { return *this = mod(*this, b); }

    bool operator == (const Number& b) const { return cmp(*this, b) == 0; }
    bool operator != (const Number& b) const { return cmp(*this, b) != 0; }
    bool operator <= (const Number& b) const { return cmp(*this, b) <= 0; }
    bool operator >= (const Number& b) const { return cmp(*this, b) >= 0; }
    bool operator <  (const Number& b) const { return cmp(*this, b) <  0; }
    bool operator >  (const Number& b) const { return cmp(*this, b) >  0; }

    Number operator + (const Number& b) const { return add(*this, b); }
    Number operator - (const Number& b) const { return sub(*this, b); }
    Number operator * (const Number& b) const { return mul(*this, b); }
    Number operator / (const Number& b) const { return div(*this, b); }
    Number operator % (const Number& b) const { return mod(*this, b); }
    Number operator - (            ) const { return Number(*this).set_neg(!neg); }

    Number& operator <<= (size_t val)
    {
        assert(val >= 0);
        auto n = (int)digits.size();
        digits.resize(n + val);
        for (int i = n-1; i >= 0; --i) digits[i+val] = digits[i];
        for (int i = 0; i < val; ++i) digits[i] = 0;
        return *this;
    }

    Number& operator >>= (size_t val)
    {
        val = std::min(val, digits.size());
        for (int i = 0; i < digits.size()-val; ++i) digits[i] = digits[i+val];
        digits.resize(digits.size() - val);
        return *this;
    }

    Number operator << (size_t val) { return Number(*this) <<= val; }
    Number operator >> (size_t val) { return Number(*this) >>= val; }

    void   resize(size_t n)  { digits.resize(n); }
    void   pop_back()        { digits.pop_back(); }
    void   push_back(type b) { digits.push_back(b); }
    size_t size() const      { return digits.size(); }
    bool   zero() const      { return digits.empty(); }
private:

    Number& truncate()
    {
        while (size() > 0 && digits.back() == 0) digits.pop_back();
        return *this;
    }

    Number& mul(type b)
    {
        type carry = 0;
        for (size_t i = 0; i < size(); i++)
        {
            type tmp = digits[i] * b + carry;
            carry = tmp / base;
            digits[i] = tmp % base;
        }
        if (carry) push_back(carry);
        return truncate();
    }

    Number& add(type b)
    {
        type carry = b;
        for (size_t i = 0; i < size() && carry; i++)
        {
            type tmp = digits[i] + carry;
            carry = tmp / base;
            digits[i] = tmp % base;
        }
        while (carry) push_back(carry % base), carry /= base;
        return truncate();
    }

    static Number add_signed(const Number& a, bool a_neg, const Number& b, bool b_neg)
    {
        if (a_neg == b_neg) return add_unsigned(a, b).set_neg(a_neg);
        if (cmp_abs(a, b) >= 0) return sub_unsigned(a, b).set_neg(a_neg);
        return sub_unsigned(b, a).set_neg(b_neg);
    }

    static Number add_unsigned(const Number& a, const Number& b)
    {
        Number result(a);
        add_unsigned(result, b);
        return result;
    }

    static Number sub_unsigned(const Number& a, const Number& b)
    {
        Number result(a);
        sub_unsigned(result, b);
        return result;
    }

    static void add_unsigned(Number& a, const Number& b)
    {
        size_t i, na = a.size(), nb = b.size(), n = std::max(na, nb);
        a.resize(n);
        type carry = 0;
        for (i = 0; i < nb; i++) carry = add_carry(&a[i], b[i] + carry);
        for (; i < n && carry; i++) carry = add_carry(&a[i], carry);
        if (carry) a.push_back(carry);
        a.truncate();
    }

    static void sub_unsigned(Number& a, const Number& b)
    {
        //assert(cmp_abs(a, b) >= 0);
        size_t i, na = a.size(), nb = b.size();
        type carry = 0;
        for (i = 0; i < nb; i++) carry = sub_carry(&a[i], b[i] + carry);
        for (; i < na && carry; i++) carry = sub_carry(&a[i], carry);
        //assert(!carry);
        a.truncate();
    }

    static type add_carry(type* a, type b)
    {
        *a += b;
        auto carry = (type)(*a >= base);
        *a -= carry * base;
        return carry;
    }

    static type sub_carry(type* a, type b)
    {
        *a -= b;
        auto carry = (type)(*a >= base);
        *a += carry * base;
        return carry;
    }

    static void div_mod(const Number& numerator, Number denominator, Number& quotient, Number& remainder)
    {
        assert(!denominator.zero());

        quotient = 0;
        remainder = numerator;

        if (cmp_abs(remainder, denominator) >= 0)
        {
            Number tmp = 1;
            auto n = (int)(remainder.size() - denominator.size());
            denominator <<= n;
            tmp <<= n;
            for (; n >= 0; n--)
            {
                while (cmp_abs(remainder, denominator) >= 0)
                {
                    sub_unsigned(remainder, denominator);
                    add_unsigned(quotient, tmp);
                }
                denominator >>= 1;
                tmp >>= 1;
            }
        }

        quotient.set_neg(numerator.neg ^ denominator.neg);
        remainder.set_neg(numerator.neg);
    }
};

#endif
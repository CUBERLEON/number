#include <iostream>
#include <ctime>

#include "Number.hpp"
#include "NumberMath.h"

int main()
{
    std::srand((unsigned int)std::time(nullptr));

    Number x, y;
    NumberMath::gcdExtended(12, 32, x, y);
    std::cout << x << " " << y << std::endl;

    std::cout << NumberMath::montgomeryMul(452346, 1234325, 13) << std::endl;

    return 0;
}
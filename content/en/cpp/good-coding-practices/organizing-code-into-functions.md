---
date: 2024-01-25 03:00:38.019002-07:00
description: 'How to: Let''s take a common task: calculating the area of a circle.
  Instead of writing the same formula every time, we encapsulate it into a function.'
lastmod: '2024-03-13T22:45:00.362749-06:00'
model: gpt-4-1106-preview
summary: Let's take a common task.
title: Organizing code into functions
weight: 18
---

## How to:
Let's take a common task: calculating the area of a circle. Instead of writing the same formula every time, we encapsulate it into a function.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Area of circle with radius " << r << " is " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Sample output:
```
Area of circle with radius 5 is 78.5397
```

## Deep Dive
Historically, procedures and functions were the backbone of structured programming, championed in the 1960s to combat the issues of "spaghetti code" in earlier imperative programming languages. Alternatives like OOP (Object-Oriented Programming) take it further by associating these functions with data structures. In C++, you've got regular functions, class methods (including static methods), lambdas, and templates functions, each offering different benefits. Implementing well-organized functions usually involves adhering to principles like DRY ("Don't Repeat Yourself") and SRP (Single Responsibility Principle), which means each function does one thing only and does it well.

## See Also
For more on functions in C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

For design principles related to functions:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Learn about lambdas and advanced function use:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures

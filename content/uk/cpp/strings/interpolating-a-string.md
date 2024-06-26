---
date: 2024-01-20 17:50:21.617948-07:00
description: "How to: \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: In C++, you can use `std::format` from C++20 to interpolate strings easily."
lastmod: '2024-04-05T22:38:48.741206-06:00'
model: gpt-4-1106-preview
summary: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438: In\
  \ C++, you can use `std::format` from C++20 to interpolate strings easily."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

## How to:
Як це зробити:

In C++, you can use `std::format` from C++20 to interpolate strings easily.

```C++
#include <iostream>
#include <format>

int main() {
    int age = 30;
    std::string name = "Oleksiy";
    std::string greeting = std::format("Hello, {0}! You are {1} years old.", name, age);
    
    std::cout << greeting << std::endl;
    
    return 0;
}
```

Output:
```
Hello, Oleksiy! You are 30 years old.
```

## Deep Dive:
Поглиблений огляд:

Before C++20, you'd typically use stream insertion operators (`<<`) or `sprintf`. These methods can be error-prone and less readable. `std::format` was introduced to simplify string formatting, inspired by Python's `str.format()` and C#'s string interpolation.

Alternatives to `std::format` include the Boost Format library or using third-party libraries like {fmt}. Internally, `std::format` uses a format string that contains replacement fields surrounded by curly braces which match the arguments by order or name.

## See Also:
Дивіться також:

- C++ `std::format` documentation: [cppreference.com](https://en.cppreference.com/w/cpp/utility/format)
- {fmt} library: [fmt.dev](https://fmt.dev/latest/index.html)
- Boost Format library: [boost.org](https://www.boost.org/doc/libs/1_75_0/libs/format/)

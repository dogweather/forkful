---
date: 2024-01-21 21:19:24.347489-07:00
description: Handling errors means planning for when things go wrong. It's vital because
  it helps avoid crashes and makes your software robust and user-friendly.
lastmod: '2024-03-11T00:14:34.233877-06:00'
model: gpt-4-1106-preview
summary: Handling errors means planning for when things go wrong. It's vital because
  it helps avoid crashes and makes your software robust and user-friendly.
title: Handling errors
---

{{< edit_this_page >}}

## What & Why?
Handling errors means planning for when things go wrong. It's vital because it helps avoid crashes and makes your software robust and user-friendly.

## How to:
Here's a basic try-catch block to handle an exception:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Oops! Something went wrong.");
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
    return 0;
}
```

Sample output:
```
Error: Oops! Something went wrong.
```

## Deep Dive
C++ has had error handling since its early days. The most basic form was checking return values. If you've been around the block, you remember the pre-standard days: C with classes and manual error checking.

Then came exceptions with C++ to give us a structured way to deal with unexpected issues. An exception is thrown with `throw` and caught with `try/catch`.

Two types of errors often crop up: logical errors, like a wrong calculation, and runtime errors, like accessing an invalid memory address. Exceptions are ideal for runtime errors. For logic errors, it's often better to use assertions or error codes.

There's an ongoing debate on exceptions vs. error codes. Exceptions can be slower and may lead to complex control flows. Error codes, while faster, can make code cluttered and harder to maintain. It's a trade-off, so knowing your use case is key.

C++17 introduced `std::optional` and `std::variant`, which are alternatives to exceptions. They’re useful for functions that may or may not return a valid result.

Exception safety can be another headache. It’s about guarantees your code provides despite exceptions. There are three levels: basic, strong, and nothrow. The more guarantees, the more complex your code might be.

Final thoughts—error handling is as much art as science. It shapes how your application survives in the wild. Don't overuse exceptions. Aim for readable, maintainable code.

## See Also
- [cppreference on exception handling](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrup's take on error handling](http://www.stroustrup.com/except.pdf)
- [C++ Core Guidelines on exceptions](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)

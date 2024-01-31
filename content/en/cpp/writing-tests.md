---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests checks if your code does what it's supposed to, catching bugs early. Programmers test to save time, headaches, and ensure reliability.

## How to:
Let's use a simple C++ function and a test using the Catch2 framework.

```cpp
// main.cpp
#define CATCH_CONFIG_MAIN  // Let Catch provide main().
#include <catch2/catch.hpp>

int Add(int a, int b) {
    return a + b;
}

TEST_CASE( "Addition works", "[math]" ) {
    REQUIRE( Add(2, 2) == 4 );
}
```
Compile with `g++ -std=c++17 main.cpp -o test -lcatch2` and run `./test`. Sample output:

```
All tests passed (1 assertion in 1 test case)
```

## Deep Dive
Testing wasn't always the norm. In the '70s, it was manual. Now, automated tests are key in agile and TDD (Test-Driven Development). Alternatives to Catch2? Google Test, Boost.Test, and CppUnit, each with unique flavors. Remember: tests assess if code meets requirements, not if those requirements are correct—that's a spec issue.

## See Also
- Catch2: https://github.com/catchorg/Catch2
- Google Test: https://github.com/google/googletest
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- CppUnit: https://freedesktop.org/wiki/Software/cppunit/

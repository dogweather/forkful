---
title:                "Writing tests"
aliases: - /en/go/writing-tests.md
date:                  2024-02-03T17:50:27.458019-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Go involves creating small, manageable pieces of code that validate the functionality and behavior of your application. Programmers write tests to ensure their code works as expected under various conditions, to facilitate refactoring, and to help prevent regressions.

## How to:

In Go, tests are typically written in the same package as the code they test. Files containing tests are named with the `_test.go` suffix. Tests are functions that take a pointer to the testing.T object (from the `testing` package) as an argument, and they signal failure by calling methods such as `t.Fail()`, `t.Errorf()`, etc.

Example of a simple test for a function `Add` defined in `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Test file `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Run your tests with the `go test` command in the same directory as your test files. Sample output indicating a passing test would look similar to:

```
PASS
ok      example.com/my/math 0.002s
```

For table-driven tests, which allow you to efficiently test various input and output combinations, define a slice of structs representing test cases:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## Deep Dive

The Go testing framework, introduced in Go 1 alongside the language itself, was designed to integrate seamlessly with the Go toolchain, reflecting Go's emphasis on simplicity and efficiency in software development. Unlike some testing frameworks in other languages that rely on external libraries or complex setups, Go's built-in `testing` package provides a straightforward way to write and run tests.

An interesting aspect of Go's approach to testing is the convention over configuration principle it adopts, like the file naming pattern (`_test.go`) and the use of standard library functionalities over external dependencies. This minimalistic approach encourages developers to write tests, as the barrier to entry is low.

While Go's built-in testing facilities cover a lot of ground, there are scenarios where third-party tools or frameworks might offer more functionalities, like mock generation, fuzz testing, or behavior-driven development (BDD) style tests. Popular libraries such as Testify or GoMock complement Go's standard testing capabilities, offering more expressive assertions or mock generation capabilities, which can be particularly useful in complex applications with many dependencies.

Despite the existence of these alternatives, the standard Go testing package remains the cornerstone for testing in Go due to its simplicity, performance, and tight integration with the language and toolchain. Whether developers choose to augment it with third-party tools or not, the Go testing framework provides a solid foundation for ensuring code quality and reliability.

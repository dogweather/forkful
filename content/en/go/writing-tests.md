---
title:                "Go recipe: Writing tests"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-tests.md"
---

{{< edit_this_page >}}

## Why Write Tests in Go Programming

Writing tests is an important aspect of software development, and it holds true in Go programming as well. Tests help in ensuring the quality and stability of the code, and they also act as a safety net for future changes. In this blog post, we will explore the importance of writing tests in Go and see how it can benefit us in the long run.

## How To Write Tests in Go

Writing tests in Go is an easy and straightforward process. Let's look at an example of a simple calculator program with addition and multiplication functions and see how we can write tests for it.

```
package main

import "fmt"

func add(x, y int) int {
    return x + y
}

func multiply(x, y int) int {
    return x * y
}

func main() {
    fmt.Println("3 + 5 =", add(3, 5))
    fmt.Println("3 * 5 =", multiply(3, 5))
}

```

To write tests, we need to create a new file with the *_test.go* extension and import the "testing" package. Then, we can write our test functions starting with the word "Test" followed by the name of the function we want to test.

```
package main

import "testing"

func TestAdd(t *testing.T) {
    result := add(3, 5)
    if result != 8 {
        t.Errorf("Addition of 3 and 5 should be 8, got %d instead", result)
    }
}

func TestMultiply(t *testing.T) {
    result := multiply(3, 5)
    if result != 15 {
        t.Errorf("Multiplication of 3 and 5 should be 15, got %d instead", result)
    }
}

```

To run these tests, we can use the "go test" command in our terminal. We should see a report with the number of tests passed and failed.

```
$ go test
PASS
ok      _/home/user/calculator      0.010s
```

Using tests, we can not only check the expected output of our functions but also ensure that they work correctly for different inputs.

## Deep Dive into Writing Tests in Go

Tests in Go follow the AAA (Arrange-Act-Assert) pattern, where we arrange the necessary variables, act upon the code under test, and assert on the expected output. This pattern makes our tests more readable and maintainable.

Go also provides a built-in testing framework, which includes functions for assertion and mocking. We can use these to make our tests more robust and comprehensive.

When writing tests in Go, we should also follow good testing practices such as writing small and independent tests, avoiding writing tests for third-party libraries, and writing tests before writing the code.

## See Also

- [Go official documentation on testing](https://golang.org/pkg/testing/)
- [Effective Go: Testing](https://golang.org/doc/effective_go#testing)
- [Writing Good Tests in Go](https://medium.com/@matryer/5-simple-tips-and-tricks-for-writing-unit-tests-in-golang-619653f90742)
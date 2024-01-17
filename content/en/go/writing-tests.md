---
title:                "Writing tests"
html_title:           "Go recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is the process of creating small pieces of code to check if the main code is working as expected. Programmers do this to ensure that their code is functional, maintainable, and free of bugs. It also helps catch any errors or unexpected behavior before the code is deployed, saving time and effort in the long run.

## How to:

Writing tests in Go is a simple and efficient process. Here are some examples to get you started:

```Go
// Create a test file with the suffix _test.go, e.g. mycode_test.go
package main

import (
    "testing" // Import testing package
    "math"
)

// Write a function to be tested
func SquareRoot(x float64) float64 {
    return math.Sqrt(x)
}

// Write a test function with the prefix Test
func TestSquareRoot(t *testing.T) {
    result := SquareRoot(16)
    if result != 4 {
        t.Errorf("SquareRoot(16) = %v; want 4", result)
    }
}
```

Running the test with `go test` should produce the following output:

```Go
--- FAIL: TestSquareRoot (0.00s)
    mycode_test.go:14: SquareRoot(16) = 5.656854249492381; want 4
FAIL
```

The output shows that the test has failed due to the incorrect expected result. This helps identify bugs or issues in the code.

More complex tests involving different scenarios and conditions can also be written using functions like `t.Run()` and assertions like `t.Errorf()`.

## Deep Dive

Writing tests has become a common practice in software development, especially with the rise of agile methodologies. It not only helps in catching errors but also aids in designing and refactoring code. In Go, tests are an integral part of the language and are executed with the `go test` command.

There are other alternative testing frameworks in Go such as Testify and Ginkgo, which provide additional features and functionalities.

When writing tests, it is important to follow good practices like naming test functions with the `Test` prefix, testing one specific behavior at a time, and using appropriate assertions. These practices help in writing maintainable tests and reduce the chances of unexpected results.

## See Also

- [Official Go documentation on testing](https://golang.org/pkg/testing/)
- [Testify framework](https://github.com/stretchr/testify)
- [Ginkgo framework](https://github.com/onsi/ginkgo)
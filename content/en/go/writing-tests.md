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

## Why

Writing tests is an essential and fundamental aspect of software development. It allows developers to ensure the functionality and stability of their code, catch bugs early on, and make future changes and updates with confidence.

## How To

Writing tests in Go is simple and straightforward. First, you need to create a test file with a name ending in `_test.go`. This distinguishes it from your regular Go code files. Then, use the `testing` package to write your tests. Here's an example of a simple test function:

```Go
func TestSum(t *testing.T) {
	result := sum(5, 10)
	if result != 15 {
		t.Errorf("Expected result to be 15, but got %d", result)
	}
}
```

The `testing` package provides the `t` parameter, which has methods for reporting test failures and logging messages. You can use the `t.Errorf()` method to report an error and halt the test execution if the result is not as expected.

To run your tests, navigate to your project directory and use the `go test` command. If all tests passed, you will see a `PASS` message, otherwise, you will see a `FAIL` message with details about which tests failed.

## Deep Dive

Go provides a robust testing framework that allows for different types of tests, such as unit tests, integration tests, and end-to-end tests. It also supports code coverage analysis to help you identify areas of your code that may need more testing.

Some best practices for writing tests in Go include using table-driven testing, where you create a table of input and expected output values for a specific function, and using meaningful test function names that describe what is being tested. You can also use the `t.Skip()` method to skip certain tests if a particular condition is not met.

In addition, Go has a built-in benchmarking tool that allows you to measure the performance of your code. You can use the `testing.B` parameter and the `b.ResetTimer()` method to reset the timer before each iteration, ensuring accurate benchmark results.

## See Also

- [Effective Go - Testing](https://golang.org/doc/effective_go#testing)
- [The Go Blog - How to Write Go Code](https://blog.golang.org/organizing-go-code)
- [Go by Example - Testing](https://gobyexample.com/testing)
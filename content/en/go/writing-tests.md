---
title:    "Go recipe: Writing tests"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential aspect of any software development process, including writing code in Go. Tests help ensure the code's functionality and catch any errors or bugs before the application is deployed. They also serve as documentation and provide confidence in the code's correctness.

## How To

To write tests in Go, we use the testing package from the Go standard library. This package provides functions and methods to create and run tests, as well as compare values and report errors. Let's take a look at a simple example:

```
func add(x, y int) int{
  return x + y
}

func TestAdd(t *testing.T){
  result := add(2, 5)
  expected := 7
  if result != expected {
    t.Errorf("Add function did not return the expected result. Got: %d, Want: %d", result, expected)
  }
}
```

In the example above, we have created a simple `add` function that takes two integers and returns their sum. We have then written a test for this function using the `testing` package. Within the `TestAdd` function, we call our `add` function with the input `2` and `5` and compare the result with the expected output of `7`. If the result is not equal to the expected output, we use the `t.Errorf` function to report an error, including the values of the result and expected output.

To run this test, we use the `go test` command in the terminal, which will execute all tests in the current directory and its subdirectories. If all tests pass, a `PASS` message will be displayed. Otherwise, any failing tests will be highlighted along with their error messages. 

We can also use the `t.Helper()` function within our test to mark it as a helper function, which can be helpful in identifying the exact location of the test failure in our code.

## Deep Dive

Besides comparing values, the `testing` package also provides other useful functions like `t.Log` and `t.FailNow`. We can use `t.Log` to print additional information during the test, such as the values of variables or any intermediate steps. And if the test fails, `t.FailNow` can be used to immediately stop the test and declare it as failed.

Another important aspect of writing tests is to create testable code. This means writing functions that can be easily tested without the need for external dependencies or user input. We can achieve this by using interfaces and mocking, which allows us to create our own testable versions of external dependencies.

## See Also

- [Writing Tests in Go](https://blog.alexellis.io/golang-writing-unit-tests/)
- [Effective Go](https://golang.org/doc/effective_go.html#testing)
- [Advanced Testing in Go](https://dev.to/quii/learn-go-by-writing-tests--m63)
---
title:    "Go recipe: Writing tests"
keywords: ["Go"]
---

{{< edit_this_page >}}

##Why Engage in Writing Tests?

Writing tests for our Go code can help us catch bugs and errors early on in the development process. This can save us time and effort in the long run by preventing major issues from arising.

##How To Write Tests in Go

Writing tests in Go is a straightforward process. First, we need to import the "testing" package into our code. Then, we can use the "func TestXxx(*testing.T)" syntax to create our tests. Let's take a look at an example:

```Go
package main

import (
    "testing"
)

func Add(x, y int) int {
    return x + y
}

func TestAdd(t *testing.T) {
    result := Add(5, 10)
    expected := 15
    if result != expected {
        t.Errorf("Expected %d, but got %d instead.", expected, result)
    }
}
```

In this code, we have created a function called "Add" that adds two integers together. Then, we have created a test called "TestAdd" to check if our function works correctly. We compare the result of Add(5, 10) to our expected value of 15. If they do not match, we use the t.Errorf() function to print an error message.

Running this test using the "go test" command will give us the following output:

```
--- FAIL: TestAdd (0.00s)
    main_test.go:15: Expected 15, but got 0 instead.
```

This tells us that our test has failed because our expected value of 15 does not match the actual result of 0. We can then go back and fix our code accordingly.

##Deep Dive into Writing Tests

Writing tests in Go is not just about checking if our code works as expected. We can also use tests to improve the overall quality and maintainability of our code. This includes testing for edge cases, writing benchmarks, and using a test-driven development approach.

One useful tool for writing tests in Go is the "go test -cover" command. This gives us a coverage report, showing which parts of our code are covered by tests. This can help us identify areas of our code that may need more testing.

Another important aspect of writing tests is using table-driven tests. This involves creating a table of test cases and their expected outputs, which can make our tests more comprehensive and easier to manage.

##See Also

- [Effective Go - Writing Tests](https://golang.org/doc/effective_go#testing)
- [Testing in Go](https://blog.golang.org/cover)
- [Table-Driven Tests in Go](https://medium.com/@matryer/5-simple-tips-and-tricks-for-writing-unit-tests-in-golang-619653f90742)

By writing tests for our Go code, we can ensure that our code is more reliable and maintainable. It may require some extra effort, but in the long run, it can save us time and headaches. Happy coding!
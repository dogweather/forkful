---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code to check if other code works. Programmers do this to catch bugs early, ensure functionality, and avoid future heartache.

## How to:

Go has a built-in testing package named `testing`. To demonstrate, suppose you have a function `Add` that sums two ints:

```Go
// add.go
package math

func Add(x, y int) int {
    return x + y
}
```

Write a test like so:

```Go
// add_test.go
package math

import (
    "testing"
)

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Run tests with `go test`. You'll see output like:

```
PASS
ok      example.com/your-module/math   0.002s
```

## Deep Dive

Go introduced built-in testing in 2011. It's simpler than using a separate library. You write tests in `_test.go` files, using `testing.T` to report failures.

Alternatives? Sure, you can use Testify for assertions, Ginkgo for BDD, or GoCheck for more advanced features. But the `testing` package is zero-dependency, easy, and often enough.

Under the hood, `go test` compiles your code and tests together, runs them, and reports results. It's idiomatic Go: common case easy, special cases possible.

## See Also

For extras, check the docs:

- Testing package: [https://pkg.go.dev/testing](https://pkg.go.dev/testing)
- Table-driven tests: [https://github.com/golang/go/wiki/TableDrivenTests](https://github.com/golang/go/wiki/TableDrivenTests)

---
title:                "编写测试"
html_title:           "Gleam: 编写测试"
simple_title:         "编写测试"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么写测试

编写测试是一种非常重要的编程技巧，它可以帮助我们提高代码的质量和稳定性。通过编写测试，我们可以更容易地发现代码中的问题，并及时修复它们。此外，编写测试还可以帮助我们更好地理解代码的功能和逻辑。

## 如何编写测试

使用 Gleam 编写测试非常简单。首先，我们需要创建一个 .gleam 文件，并在其中引入 `test` 库。然后，我们可以使用 `test_module` 和 `test_case` 函数来创建测试模块和测试用例。最后，我们使用 `assert` 函数来断言测试的结果是否符合预期。

```Gleam
import test

test_module("Examples", [
  test_case("Addition", {
    let result = 2 + 2
    assert(result == 4)
  })
])
```

运行上面的测试文件，我们可以得到如下输出：

```
Failing Examples (1 failures)

✕ Addition

Reason:
assert failed
```

这表示我们的测试失败了，因为 `2 + 2` 并不等于 `4`。但是我们可以很轻松地修改代码来让测试通过。

## 深入了解测试

编写测试时，我们可以使用多种断言函数来判断不同类型的值是否相等，例如 `assert_equal`、`assert_not_equal`、`assert_true` 等。此外，我们还可以使用 `test_expectation` 函数来测试是否会抛出预期的错误。

下面是一个更复杂的测试例子，它测试了一个计算阶乘的函数：

```Gleam
import test

fn factorial(n) {
  if n == 0 {
    1
  } else {
    n * factorial(n - 1)
  }
}

test_module("Factorial", [
  test_case("0! should equal 1", {
    assert_equal(factorial(0), 1)
  }),
  test_case("5! should equal 120", {
    assert_equal(factorial(5), 120)
  }),
  test_case("Negative numbers should throw an exception", {
    assert_error(test_expectation(Err, factorial(-5)))
  })
])
```

通过编写测试，我们可以确保我们的函数在各种情况下都能正确地工作。测试还可以帮助我们随时检查修改代码后是否对功能产生了负面影响。

# 参考链接

- [Gleam 官方文档（中文）](https://gleam.run/book/zh-CN/)
- [测试在 Swift 中的应用](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/04-writing_tests.html#//apple_ref/doc/uid/TP40014132-CH4-SW1)
---
title:    "Gleam: 编写测试"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 为什么要编写测试？

编写测试是软件开发中至关重要的一部分。通过编写测试，开发人员可以验证他们的代码是否按照预期工作，避免在部署到生产环境后出现意外的错误。同时，测试还可以帮助开发人员更深入地理解自己的代码，从而提高代码质量和可维护性。

## 如何编写Gleam测试？

要编写Gleam测试，首先需要在代码中引入标准库中的测试模块。然后，在函数名前面加上`#[test]`注解，以标识该函数是一个测试函数。接下来，在函数体中使用断言来验证代码的输出是否符合预期。最后，使用`test()`函数来执行所有的测试函数。

```Gleam
import gleam/test

#[test]
fn test_addition() {
  assert 1 + 1 == 2
}

#[test]
fn test_multiply() {
  assert 2 * 3 == 6
}

test()
```

运行以上代码，如果所有的断言都通过，就表示测试通过了。

## 深入了解编写测试

编写测试时，需要尽可能覆盖所有可能的输入和情况，以保证代码的稳定性。同时，要注意不要编写过多的测试，以免增加不必要的开发时间和开销。

除了简单的断言外，Gleam还提供了丰富的测试工具，如`assert_eq()`、`assert_not_eq()`等，可以根据具体的需求选择合适的方法来验证代码的输出。

# 参考资料

- Gleam测试文档：https://gleam.run/book/testing.html
- 编写测试的最佳实践：https://stackify.com/unit-testing-basics-best-practices/ 
- 深入理解Gleam断言：https://gleam.run/book/tests/assert.html

# 参见

- [编写可测试代码的技巧](https://verygood.ventures/blog/code-coverage-100)
- [有效编写测试的方法](https://testing.googleblog.com/2015/04/just-say-no-to-more-end-to-end-tests.html)
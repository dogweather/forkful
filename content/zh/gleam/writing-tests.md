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

# 写测试是什么 & 为什么要这么做？

写测试是指针对程序进行一系列的测试，来验证代码的正确性。它是程序员们常用的一种手段，可以确保程序的质量，并且减少未来出现的错误。

# 如何进行测试：

```Gleam
fn my_function(x) {
  x + 5
}

describe "My Function" {
  it "returns the input plus 5" {
    expect(my_function(3)) == 8
  }
}
```

在这个例子中，我们定义了一个函数 `my_function` 来对输入进行计算操作，并使用 `describe` 和 `it` 来定义我们的测试。当测试运行时，我们会得到一个输出，来验证函数是否按照预期进行计算。

# 深入探讨：

写测试已经成为现代软件开发中不可或缺的一部分。它可以帮助我们更早地发现错误，并且在修改代码时提供一个安全的保障。与其他语言相比，Gleam 的测试特别容易理解和管理。

# 参考资料：

- [Gleam语言官方网站](https://gleam.run/)
- [关于写测试的更多信息](https://www.freecodecamp.org/news/software-testing-explained-dbe743def859/)
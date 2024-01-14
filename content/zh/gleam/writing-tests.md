---
title:                "Gleam: 编写测试"
simple_title:         "编写测试"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-tests.md"
---

{{< edit_this_page >}}

为什么: 为什么要参与写测试？写测试能够帮助开发者在编写代码时发现错误，从而减少故障和维护时间。

如何: 以下是一个示例代码块，展示如何使用Gleam编写测试，并展示输出结果。

```Gleam
import gleam/test/assert
import my_module

suite "My Module Tests" {
  test "add two numbers" {
    assert.equal(my_module.add(2, 3), 5)
  }

  test "multiply two numbers" {
    assert.equal(my_module.multiply(4, 5), 20)
  }
}
```

输出结果将会显示测试通过或失败的结果以及具体的失败信息。通过编写多个测试来覆盖代码的不同情况，可以确保代码在各种情况下都能正确运行。

深入了解: 编写测试可以帮助开发者更加自信地修改和重构代码，因为他们可以通过运行测试来保证代码的正确性。此外，写测试还能够帮助开发者在添加新功能或修复bug时提前发现潜在的问题，从而提高开发效率和代码质量。

另外，编写测试也是一种良好的编程习惯，能够让开发者在写代码时更加注重代码的可测试性和可维护性。

## 参考链接

- Gleam官方文档：https://gleam.run/
- GitHub：https://github.com/gleam-lang/gleam
- 博客文章：https://www.gleam.run/news/writing-tests/
- 视频教程：https://www.youtube.com/watch?v=Zxh55zOSbb8

看看这些资源可以帮助你更加深入地了解如何使用Gleam编写测试，并充分利用这项工具来改善你的代码质量！
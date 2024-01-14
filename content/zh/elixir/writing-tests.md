---
title:                "Elixir: 编写测试 (Biānxiě cèshì)"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么需要写测试 (Why)

编写测试是软件开发过程中必不可少的一部分。通过编写测试，我们可以验证代码的正确性，减少错误和问题，提高代码质量和稳定性。

## 如何编写测试 (How To)

编写测试可以分为两步：1）编写代码；2）运行测试。首先，我们需要创建一个测试文件，命名为 "example_test.exs"。然后，在文件中导入 "ExUnit" 的模块，并定义一个测试模块，例如 "ExampleTest"。在该模块中，可以使用 "test" 函数来编写具体的测试代码。

```Elixir
defmodule ExampleTest do
  use ExUnit.Case
  test "addition" do
    assert 1 + 1 == 2
  end
end
```

运行测试的方法有多种，可以使用命令行工具 "mix test"，也可以在编辑器中使用相应的插件来运行测试。

测试代码的输出结果如下所示：

```
1) test addition (ExampleTest)
     test/example_test.exs:3
      Assertion with == failed
      code:  assert 1 + 1 == 2
      left:  2
      right: 3

Finished in 0.07 seconds
1 test, 1 failure

Randomized with seed 52289
```

## 深入了解测试 (Deep Dive)

编写测试的目的是为了验证代码的正确性，在测试中通常会包含断言(assertion)，即我们预期的代码结果。除了基本的等式判断，我们还可以使用其他函数来编写更加复杂的测试。例如，可以使用 "assert_raise" 函数来测试代码是否会抛出异常，使用 "assert_receive" 函数来测试代码是否可以正确接收消息。

此外，我们也可以编写 "setup" 和 "teardown" 函数，用于准备测试环境和清理测试环境，在测试代码之间保持独立性。

## 参考链接 (See Also)

- [ExUnit 指南](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir 测试实践](https://hexdocs.pm/ex_unit/writing-tests.html)
- [Elixir 断言函数示例](https://hexdocs.pm/ex_unit/ExUnit.Assertions.html)
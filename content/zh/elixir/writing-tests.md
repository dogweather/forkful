---
title:                "Elixir: 编写测试"
simple_title:         "编写测试"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

在编写代码的过程中，我们经常会遇到各种各样的问题。而写测试就可以帮助我们在开发过程中发现和解决这些问题，同时也可以确保我们的代码质量和稳定性。

## 如何

为了写测试，我们需要使用 Elixir 的测试框架 ExUnit。下面是一个简单的示例，展示了如何编写测试来测试一个函数是否返回正确的结果。

```Elixir
defmodule Math do
  def add(a, b) do
    a + b
  end
end
```

```Elixir
defmodule MathTest do
  use ExUnit.Case

  test "adds two numbers" do
    assert Math.add(1, 2) == 3
  end
end
```

运行测试命令 `mix test` 后，我们就可以看到测试结果输出为 `1 test, 0 failures`，表示测试通过。

## 深入探讨

除了简单的测试，我们还可以在测试中使用一些特殊的断言函数，来确保代码的正确性。例如，可以使用 `assert_raise` 来测试一个函数是否会抛出异常。

```Elixir
test "raises an error if dividing by zero" do
  assert_raise ArgumentError, fn ->  
    Math.div(10, 0)
  end
end
```

另外，我们也可以在测试中使用 `setup` 和 `teardown` 函数来准备和清理测试所需的环境，以及使用 `async: true` 参数来并发运行测试。

## 参考链接

- [Elixir官方文档 - 测试](https://elixir-lang.org/getting-started/introduction.html#testing)
- [ExUnit文档](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - 测试](https://elixirschool.com/zh-hans/lessons/basics/testing/)

## 更多阅读

- [如何使用 ExCoveralls 测试覆盖率工具](https://cloud.tencent.com/developer/article/1518689)
- [5个步骤编写高质量的 Elixir 测试代码](https://medium.com/@mikulskibartosz/writing-great-elixir-tests-in-5-easy-steps-f62f6cc825b6)
- [Elixir与Clojure测试比较](https://blog.rowanhu.com/2017/01/18/clojure%E4%B8%8Eelixir%E7%9A%84%E6%B5%8B%E8%AF%95%E6%AF%94%E8%BE%83/)
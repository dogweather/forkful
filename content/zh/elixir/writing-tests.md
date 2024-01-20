---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
编写测试是创建用于自动验证代码功能的脚本。程序员这样做是为了确保程序按预期运行，并在未来的开发中防止回归错误。

## How to: 怎么做
以 Elixir 中的 ExUnit 为例，这是它的主要测试框架。你可以创建一个测试文件，并使用 `test` 宏来编写测试用例。

```elixir
# test/my_app_test.exs

ExUnit.start()

defmodule MyAppTest do
  use ExUnit.Case

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

运行测试后的输出：

```shell
..

Finished in 0.04 seconds
1 test, 0 failures
```

## Deep Dive 深入探索
ExUnit 是 Elixir 语言自带的测试库，由 José Valim 和其他贡献者开发，以确保 Elixir 程序的可靠性和健壮性。在 Elixir 圈里，还有如 `Espec` 的替代框架，它受 RSpec 启发。ExUnit 采用了一个模块化的设计，让开发人员可以灵活地添加自定义功能。

## See Also 另请参阅
- Elixir 官网的测试指南: [Elixir Testing Introduction](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#testing)
- ExUnit 文档: [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- Espec GitHub: [Espec on GitHub](https://github.com/antonmi/espec)
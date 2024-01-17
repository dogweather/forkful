---
title:                "编写测试"
html_title:           "Elixir: 编写测试"
simple_title:         "编写测试"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-tests.md"
---

{{< edit_this_page >}}

#写测试是什么&为什么？
写测试是编写用于检测代码功能的小型程序的过程。程序员会这样做是为了确保代码在修改后仍然能够正常工作，并且可以帮助追踪和修复错误。

#如何：
下面的代码块展示了如何在Elixir中编写测试的示例。代码块下方是运行该测试的示例输出。

```elixir
defmodule MyMath do
  def sum(a, b) do
    a + b
  end
end
```

```elixir
defmodule MyMathTest do
  use ExUnit.Case
  test "sum/2 returns correct result" do
    assert MyMath.sum(2, 3) == 5
  end
end
```

```
$ mix test
.Compiling 1 file (.ex)
.
Finished in 0.03 seconds (0.02s on load, 0.01s on tests)
1 tests, 0 failures
```

#深入了解：
写测试的历史可追溯到早期的软件工程实践中，它已被证明是一种有效的工具。除了Elixir的测试框架ExUnit外，还有其他的框架可以用来编写测试，如ESpec、PropEr等。测试通常包括单元测试、集成测试和端到端测试，每种测试都有不同的用途和优缺点。写测试时，还需要考虑测试覆盖率、测试驱动开发等因素。

#相关资源：
- Elixir官方文档：https://hexdocs.pm/elixir/1.11/ExUnit.html
- ExUnit源码：https://github.com/elixir-lang/elixir/blob/master/lib/ex_unit/
- ESpec：https://github.com/lucasmazza/espec
- PropEr：https://github.com/manopapad/proper
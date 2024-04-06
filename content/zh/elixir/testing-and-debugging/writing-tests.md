---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:19.288186-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Elixir\u4F7F\u7528ExUnit\u4F5C\u4E3A\u5176\
  \u5185\u7F6E\u6D4B\u8BD5\u6846\u67B6\uFF0C\u5B83\u65E2\u5F3A\u5927\u53C8\u6613\u4E8E\
  \u4F7F\u7528\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u4F8B\u5B50\uFF1A 1.\u2026"
lastmod: '2024-04-05T21:53:47.704125-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u4F7F\u7528ExUnit\u4F5C\u4E3A\u5176\u5185\u7F6E\u6D4B\u8BD5\u6846\
  \u67B6\uFF0C\u5B83\u65E2\u5F3A\u5927\u53C8\u6613\u4E8E\u4F7F\u7528\u3002\u8FD9\u91CC\
  \u6709\u4E00\u4E2A\u57FA\u672C\u4F8B\u5B50\uFF1A 1."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何操作:
Elixir使用ExUnit作为其内置测试框架，它既强大又易于使用。这里有一个基本例子：

1. 在你的Elixir项目的`test`目录中创建一个新的测试文件。例如，如果您正在测试一个名为`MathOperations`的模块，您的测试文件可以是`test/math_operations_test.exs`。

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # 这是一个简单的测试案例，以检查加法函数
  test "两个数字的加法" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

要运行你的测试，请在终端中使用`mix test`命令。如果`MathOperations.add/2`函数正确地加了两个数字，你将看到类似以下的输出：

```
..

完成于0.03秒
1个测试, 0个失败
```

对于涉及外部服务或API的测试，你可能想使用模拟库，例如`mox`，以避免实际触及服务：

1. 在`mix.exs`中将`mox`添加到你的依赖项：

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # 其他依赖...
  ]
end
```

2. 在你的测试助手中定义一个模拟模块(`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. 在你的测试案例中使用模拟：

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # 这告诉Mox验证此模拟是否如预期般被调用
  setup :verify_on_exit!

  test "从API获取数据" do
    # 设置模拟响应
    expect(HTTPClientMock, :get, fn _url -> {:ok, "模拟响应"} end)
    
    assert SomeAPIClient.get_data() == "模拟响应"
  end
end
```

当运行`mix test`时，此设置允许你将单元测试与真实的外部依赖隔离，专注于你自己代码的行为。这种模式确保你的测试运行迅速并保持可靠，无论外部服务状态或互联网连接如何。

---
aliases:
- /zh/elixir/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:19.288186-07:00
description: "\u5728Elixir\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u81EA\
  \u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u7684\u884C\u4E3A\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u8BC1\u8D28\u91CF\
  \uFF0C\u9632\u6B62\u56DE\u5F52\uFF0C\u5E76\u4FC3\u8FDB\u4EE3\u7801\u91CD\u6784\uFF0C\
  \u4F7F\u5F00\u53D1\u8FC7\u7A0B\u66F4\u52A0\u53EF\u9760\u548C\u9AD8\u6548\u3002"
lastmod: 2024-02-18 23:08:58.869066
model: gpt-4-0125-preview
summary: "\u5728Elixir\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u81EA\
  \u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u7684\u884C\u4E3A\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u8BC1\u8D28\u91CF\
  \uFF0C\u9632\u6B62\u56DE\u5F52\uFF0C\u5E76\u4FC3\u8FDB\u4EE3\u7801\u91CD\u6784\uFF0C\
  \u4F7F\u5F00\u53D1\u8FC7\u7A0B\u66F4\u52A0\u53EF\u9760\u548C\u9AD8\u6548\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么 & 为什么?
在Elixir中编写测试涉及创建自动化脚本来验证您的代码的行为。程序员这样做是为了保证质量，防止回归，并促进代码重构，使开发过程更加可靠和高效。

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

---
title:                "处理XML"
aliases:
- zh/elixir/working-with-xml.md
date:                  2024-01-26T04:29:50.982177-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
在 Elixir 中处理 XML 意味着解析、创建和操作 XML 数据。程序员处理 XML 是因为它在网络服务、配置文件和遗留系统中广泛存在。

## 如何操作:
Elixir 的标准库中不包括 XML 解析。SweetXML 是一个流行的选择。以下是如何使用它的方法：

```elixir
# 在 mix.exs 中添加 SweetXML 到你的依赖
{:sweet_xml, "~> 0.6"}

# 在你的代码中
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# 解析 XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # 输出：Tove
```

## 深入探讨
XML，或者说可扩展标记语言，自 90 年代末就已经出现。它虽然冗长，但结构严谨——非常适合复杂数据交换。尽管 JSON 因其简单性而流行度飙升，XML 在许多企业和金融系统中因其表达性和标准化模式而依然根深蒂固。

替代方案包括：
- JSON，用于更轻量级、不那么冗长的数据交换。
- Protobuf 或 Thrift，用于二进制序列化数据通讯，特别是对于内部系统。

在底层，Elixir 的 XML 库利用 Erlang 的 :xmerl 库进行解析，它提供了强大的支持，但可能不如更现代的方法直观。随着 Elixir 的发展，像 SweetXML 这样的社区驱动库以更具 Elixir 风格的语法包装了这些功能，使得 XML 操作更加容易。

## 参见:
- SweetXML 在 Hex 上：https://hex.pm/packages/sweet_xml
- Elixir 对 XML 解析的看法：https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl 文档，用于底层 XML 处理：http://erlang.org/doc/apps/xmerl/index.html

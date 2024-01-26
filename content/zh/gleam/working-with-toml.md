---
title:                "使用TOML"
date:                  2024-01-26T04:22:02.456932-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
使用 TOML 意味着用代码解析和生成 TOML（Tom 的显而易见的最小化语言）文件。程序员之所以使用 TOML，是因为它易于阅读的配置文件和数据序列化，感谢其清晰的语义和与传统数据类型的兼容性。

## 如何操作：
Gleam 没有内置的 TOML 支持，因此你需要一个外部库。举个例子：

```gleam
// 假设你有一个 TOML 解析库：
import toml/{Parser, Encoder}

// 解析 TOML 内容
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// 使用解析后的数据
match parsed {
  Ok(data) -> "数据解析成功！"
  Error(_) -> "数据解析失败。"
}

// 从 Gleam 数据结构生成 TOML 内容
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

示例输出：

```
数据解析成功！
```

## 深入了解
TOML 由 Tom Preston-Werner 在 2013 年发布。其目标：比 XML 更易读、更直接，而且比 YAML 文件配置的复杂性要低。尽管简单，但它对于结构化数据来说非常健壮，提供了明确且易于理解的语法。选择包括 JSON、YAML 和 INI，但是 TOML 的简约和清晰的语法通常在配置文件中胜出。在 Gleam 中实现 TOML 包括两个主要行动：将 TOML 解析为本机数据结构，并将本机数据结构序列化为 TOML。由于 Gleam 与 BEAM 语言的互操作性，可以在 Gleam 中使用大多数 Erlang 或 Elixir 的 TOML 库，确保 Gleam 项目的无缝集成。

## 另请参阅
- TOML 语言规范：[https://toml.io/en/](https://toml.io/en/)
- 一个 Erlang TOML 解析器：[https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML 在 GitHub 上：[https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
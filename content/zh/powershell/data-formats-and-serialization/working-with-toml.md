---
title:                "使用TOML"
aliases:
- /zh/powershell/working-with-toml.md
date:                  2024-01-26T04:25:21.581018-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

TOML，即 Tom's Obvious, Minimal Language（汤姆的明显、最小化语言），是一种数据序列化格式，因其清晰的语义而易于阅读。程序员使用它作为配置文件，因为它在人类可读性和机器友好性之间取得了平衡。

## 如何操作：

在 PowerShell 中，没有原生的 cmdlet 来解析 TOML。如果您想在 PowerShell 中使用 TOML，通常会使用一个模块或使用像 `toml-to-json` 这样的工具将 TOML 转换为 JSON。以下是使用虚构模块 `PowerShellTOML` 的操作方法：

```PowerShell
# 首先，安装模块（假想的，用于演示）
Install-Module PowerShellTOML

# 导入一个 TOML 文件
$config = Import-TomlConfig -Path './config.toml'

# 访问一个值
Write-Output $config.database.server

# 'config.toml' 中的示例 TOML 内容：
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# 示例输出：
# 192.168.1.1
```

## 深入了解

TOML 是由 GitHub 的联合创始人 Tom Preston-Werner 创建的，作为 XML 和 YAML 配置文件的一个更简单的替代品。其第一个版本出现在 2013 年。TOML 可以与 JSON 相提并论，但旨在更加人性化，使其成为由人维护的配置的好选择。替代品包括 YAML、JSON 和 XML。

就实现而言，一个 PowerShell 的 TOML 模块通常会是围绕一个用更注重性能的语言（如 C#）编写的 TOML 库的包装器。PowerShell 没有内建的 TOML 支持，这就是为什么这样一个模块对于方便地与 TOML 格式交互是必要的。

## 另请参阅

- TOML 标准：https://toml.io/en/
- `toml` PowerShell 模块的 GitHub 仓库（如果阅读时存在）：https://github.com/powershell/PowerShellTOML
- TOML 介绍：https://github.com/toml-lang/toml
- 数据序列化格式比较：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats

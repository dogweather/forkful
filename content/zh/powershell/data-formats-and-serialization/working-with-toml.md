---
date: 2024-01-26 04:25:21.581018-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 PowerShell \u4E2D\uFF0C\u6CA1\u6709\
  \u539F\u751F\u7684 cmdlet \u6765\u89E3\u6790 TOML\u3002\u5982\u679C\u60A8\u60F3\u5728\
  \ PowerShell \u4E2D\u4F7F\u7528 TOML\uFF0C\u901A\u5E38\u4F1A\u4F7F\u7528\u4E00\u4E2A\
  \u6A21\u5757\u6216\u4F7F\u7528\u50CF `toml-to-json` \u8FD9\u6837\u7684\u5DE5\u5177\
  \u5C06 TOML \u8F6C\u6362\u4E3A JSON\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\u865A\u6784\
  \u6A21\u5757 `PowerShellTOML` \u7684\u64CD\u4F5C\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:48.337266-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

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

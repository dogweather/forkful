---
date: 2024-01-26 04:25:21.581018-07:00
description: "TOML\uFF0C\u5373 Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u5C0F\u5316\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u6570\
  \u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u56E0\u5176\u6E05\u6670\u7684\u8BED\u4E49\
  \u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4F5C\u4E3A\
  \u914D\u7F6E\u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u5728\u4EBA\u7C7B\u53EF\u8BFB\u6027\
  \u548C\u673A\u5668\u53CB\u597D\u6027\u4E4B\u95F4\u53D6\u5F97\u4E86\u5E73\u8861\u3002"
lastmod: '2024-03-13T22:44:48.041589-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373 Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u5C0F\u5316\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u6570\
  \u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u56E0\u5176\u6E05\u6670\u7684\u8BED\u4E49\
  \u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4F5C\u4E3A\
  \u914D\u7F6E\u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u5728\u4EBA\u7C7B\u53EF\u8BFB\u6027\
  \u548C\u673A\u5668\u53CB\u597D\u6027\u4E4B\u95F4\u53D6\u5F97\u4E86\u5E73\u8861\u3002\
  ."
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

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:47.940621-07:00
description: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u662F\u7BA1\u7406\u548C\u64CD\u4F5C\u7ED3\u6784\u5316\u3001\u8868\u683C\u5F62\u5F0F\
  \u6570\u636E\u7684\u5E38\u89C1\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\
  \u884C\u6B64\u64CD\u4F5C\u4EE5\u6709\u6548\u5730\u5BFC\u5165\u3001\u5BFC\u51FA\u6216\
  \u64CD\u4F5C\u6570\u636E\uFF0C\u7528\u4E8E\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\uFF0C\
  \u4F8B\u5982\u6570\u636E\u5206\u6790\u3001\u62A5\u544A\uFF0C\u751A\u81F3\u4E3A\u7F51\
  \u9875\u5E94\u7528\u7A0B\u5E8F\u63D0\u4F9B\u52A8\u529B\u3002"
lastmod: '2024-02-25T18:49:45.607905-07:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u662F\
  \u7BA1\u7406\u548C\u64CD\u4F5C\u7ED3\u6784\u5316\u3001\u8868\u683C\u5F62\u5F0F\u6570\
  \u636E\u7684\u5E38\u89C1\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\
  \u6B64\u64CD\u4F5C\u4EE5\u6709\u6548\u5730\u5BFC\u5165\u3001\u5BFC\u51FA\u6216\u64CD\
  \u4F5C\u6570\u636E\uFF0C\u7528\u4E8E\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\uFF0C\u4F8B\
  \u5982\u6570\u636E\u5206\u6790\u3001\u62A5\u544A\uFF0C\u751A\u81F3\u4E3A\u7F51\u9875\
  \u5E94\u7528\u7A0B\u5E8F\u63D0\u4F9B\u52A8\u529B\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么以及为什么？

处理 CSV（逗号分隔值）文件是管理和操作结构化、表格形式数据的常见任务。程序员经常执行此操作以有效地导入、导出或操作数据，用于各种应用程序，例如数据分析、报告，甚至为网页应用程序提供动力。

## 如何操作：

### 读取 CSV 文件

要从 CSV 文件中读取数据，请使用 `Import-Csv` 命令。此命令读取文件并将其转换为自定义的 PowerShell 对象，每行一个。

```powershell
# 导入 CSV 文件
$data = Import-Csv -Path "C:\Data\users.csv"
# 显示内容
$data
```

**示例输出：**

```
名字    年龄    城市
----    ---    ----
John    23     纽约
Doe     29     洛杉矶
```

### 写入 CSV 文件

相反地，要将数据写入 CSV 文件，请使用 `Export-Csv` 命令。此命令接受输入对象并将其转换为 CSV 格式。

```powershell
# 创建一个要导出的对象
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# 导出到 CSV 文件
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

执行后，将创建一个名为 `new_users.csv` 的文件，包含所提供的数据。

### 筛选和操作 CSV 内容

要筛选或操作 CSV 文件中的数据，请使用 PowerShell 的对象操作功能。例如，仅选取某个年龄以上且来自特定城市的用户：

```powershell
# 导入并筛选数据
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# 显示筛选后的数据
$filteredData
```

**示例输出：**

```
名字    年龄    城市
----    ---    ----
Doe     29     洛杉矶
```

### 使用第三方库

虽然 PowerShell 的原生命令通常足以完成常见任务，但更复杂的操作可能会受益于第三方库或工具。然而，对于标准的 CSV 操作，如读取、写入、筛选或排序，PowerShell 的内建命令如 `Import-Csv` 和 `Export-Csv` 通常提供了强大的功能，无需额外的库。

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:47.940621-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:48.040413-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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

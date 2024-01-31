---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV，即逗号分隔值，简单且广泛用于存储表格数据。程序员处理CSV文件，因为它们易于阅读，兼容性好，且易于与表格软件、数据库和编程语言交云。

## How to:
### 读取CSV
```PowerShell
# 读取CSV文件到变量
$data = Import-Csv -Path "example.csv"

# 显示数据
$data
```
输出样例：
```
Name        Age
----        ---
Alice       23
Bob         29
Charlie     35
```

### 写入CSV
```PowerShell
# 创建对象
$person = [PSCustomObject]@{Name='David'; Age=42}

# 将对象写入CSV文件
$person | Export-Csv -Path "output.csv" -NoTypeInformation

# 查看结果
Get-Content "output.csv"
```
输出样例：
```
"Name","Age"
"David","42"
```

## Deep Dive
历史背景：CSV格式早在1970年代就已被使用，因其简单性快速成为交换表格数据的标准。

替代品：尽管JSON和XML在处理复杂数据时更常用，但它们没有像CSV那样易于人类阅读。

实现细节：PowerShell处理CSV文件时，Import-Csv将数据作为对象集合读取，Export-Csv将对象集合写入到CSV文件。每行代表一个对象，每个逗号分隔的值对应一个属性。

## See Also
- PowerShell 文档中对CSV的官方介绍：[About Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- CSV格式标准：[RFC 4180](https://tools.ietf.org/html/rfc4180)

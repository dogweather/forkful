---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON（JavaScript Object Notation）是一种数据交换格式。程序员用它因为它简单，易读且易于和Web应用交互。

## How to:
### 读取JSON文件
```PowerShell
$json = Get-Content -Path 'example.json' | ConvertFrom-Json
```
### 转换对象为JSON字符串
```PowerShell
$obj = @{name='张三'; age=30}
$jsonString = $obj | ConvertTo-Json
```
### 输出样例
```PowerShell
echo $jsonString
```
输出：
```json
{
    "name":  "张三",
    "age":  30
}
```

## Deep Dive
JSON起源于2001年，目的是为了使web应用的数据交互更加高效。与其它数据格式（如XML）比较，JSON更轻量级，也易于转换成JavaScript对象。PowerShell内置了对JSON的支持，主要通过`ConvertFrom-Json`和`ConvertTo-Json` cmdlets实现。

## See Also
- 官方JSON网站: [www.json.org](https://www.json.org/json-en.html)
- PowerShell官方文档: [PowerShell JSON cmdlets](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/)
- JSON与XML比较: [Comparing JSON and XML](https://www.w3schools.com/js/js_json_xml.asp)

---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:23:30.836267-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

PowerShell与JSON（JavaScript对象表示法）的整合，关于解析（读取）和生成（写入）JSON数据，这是网络上数据交换的常见格式。程序员使用JSON来与Web API互动、操作配置文件，或促进不同语言和平台之间的数据交互，因为它具有轻量级和语言无关的特点。

## 如何操作：

### 解析JSON

要在PowerShell中读取或解析JSON，你可以使用`ConvertFrom-Json` cmdlet。给定一个JSON字符串，此cmdlet会将其转换为一个PowerShell对象。

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

示例输出：

```
John Doe
```

此示例演示了如何解析一个简单的JSON字符串，以访问结果对象的属性。

### 生成JSON

要从PowerShell对象生成JSON，你可以使用`ConvertTo-Json` cmdlet。这对于准备要发送给Web服务或保存到配置文件中的数据很方便。

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

示例输出：

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

这段代码创建了一个PowerShell对象，然后将其转换为JSON字符串。

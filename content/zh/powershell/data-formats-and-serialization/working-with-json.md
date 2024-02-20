---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:30.836267-07:00
description: "PowerShell\u4E0EJSON\uFF08JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\
  \u7684\u6574\u5408\uFF0C\u5173\u4E8E\u89E3\u6790\uFF08\u8BFB\u53D6\uFF09\u548C\u751F\
  \u6210\uFF08\u5199\u5165\uFF09JSON\u6570\u636E\uFF0C\u8FD9\u662F\u7F51\u7EDC\u4E0A\
  \u6570\u636E\u4EA4\u6362\u7684\u5E38\u89C1\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528JSON\u6765\u4E0EWeb API\u4E92\u52A8\u3001\u64CD\u4F5C\u914D\u7F6E\u6587\u4EF6\
  \uFF0C\u6216\u4FC3\u8FDB\u4E0D\u540C\u8BED\u8A00\u548C\u5E73\u53F0\u4E4B\u95F4\u7684\
  \u6570\u636E\u4EA4\u4E92\uFF0C\u56E0\u4E3A\u5B83\u5177\u6709\u8F7B\u91CF\u7EA7\u548C\
  \u8BED\u8A00\u65E0\u5173\u7684\u7279\u70B9\u3002"
lastmod: 2024-02-19 22:05:07.091899
model: gpt-4-0125-preview
summary: "PowerShell\u4E0EJSON\uFF08JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\
  \u7684\u6574\u5408\uFF0C\u5173\u4E8E\u89E3\u6790\uFF08\u8BFB\u53D6\uFF09\u548C\u751F\
  \u6210\uFF08\u5199\u5165\uFF09JSON\u6570\u636E\uFF0C\u8FD9\u662F\u7F51\u7EDC\u4E0A\
  \u6570\u636E\u4EA4\u6362\u7684\u5E38\u89C1\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528JSON\u6765\u4E0EWeb API\u4E92\u52A8\u3001\u64CD\u4F5C\u914D\u7F6E\u6587\u4EF6\
  \uFF0C\u6216\u4FC3\u8FDB\u4E0D\u540C\u8BED\u8A00\u548C\u5E73\u53F0\u4E4B\u95F4\u7684\
  \u6570\u636E\u4EA4\u4E92\uFF0C\u56E0\u4E3A\u5B83\u5177\u6709\u8F7B\u91CF\u7EA7\u548C\
  \u8BED\u8A00\u65E0\u5173\u7684\u7279\u70B9\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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

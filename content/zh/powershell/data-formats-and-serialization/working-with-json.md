---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:30.836267-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728PowerShell\u4E2D\u8BFB\u53D6\
  \u6216\u89E3\u6790JSON\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`ConvertFrom-Json` cmdlet\u3002\
  \u7ED9\u5B9A\u4E00\u4E2AJSON\u5B57\u7B26\u4E32\uFF0C\u6B64cmdlet\u4F1A\u5C06\u5176\
  \u8F6C\u6362\u4E3A\u4E00\u4E2APowerShell\u5BF9\u8C61\u3002"
lastmod: '2024-03-13T22:44:48.039051-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u5728PowerShell\u4E2D\u8BFB\u53D6\u6216\u89E3\u6790JSON\uFF0C\u4F60\
  \u53EF\u4EE5\u4F7F\u7528`ConvertFrom-Json` cmdlet\u3002\u7ED9\u5B9A\u4E00\u4E2A\
  JSON\u5B57\u7B26\u4E32\uFF0C\u6B64cmdlet\u4F1A\u5C06\u5176\u8F6C\u6362\u4E3A\u4E00\
  \u4E2APowerShell\u5BF9\u8C61."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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

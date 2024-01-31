---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (什么是JSON以及为什么使用)
JSON，即JavaScript对象表示法，是轻量级的数据交换格式。程序员用JSON来传输和存储数据，简单，易读写，易于转换成代码对象。

## How to: (如何操作)
你可以使用`System.Text.Json`处理JSON。先添加：
```csharp
using System.Text.Json;
```
要序列化对象，写：
```csharp
var player = new { Name = "Alice", Score = 100 };
string json = JsonSerializer.Serialize(player);
Console.WriteLine(json);
// 输出: {"Name":"Alice","Score":100}
```
反序列化JSON，这样：
```csharp
var json = "{\"Name\":\"Alice\",\"Score\":100}";
var player = JsonSerializer.Deserialize<dynamic>(json);
Console.WriteLine(player.Name); // 输出: Alice
```

## Deep Dive (深入探讨)
JSON自2001年诞生，已成为XML的现代替代品。`.NET`之前主要用`Newtonsoft.Json`，即Json.NET。`System.Text.Json`是.NET Core 3.0后的新选项，性能更好，内存使用更低。深入了解时，考虑性能、扩展性和安全性。

## See Also (另请参阅)
- 官方`System.Text.Json`文档：[docs.microsoft.com](https://docs.microsoft.com/zh-cn/dotnet/standard/serialization/system-text-json-overview)
- JSON介绍及其语法：[json.org/json-zh.html](http://json.org/json-zh.html)
- Json.NET官方网站：[newtonsoft.com/json](https://www.newtonsoft.com/json)

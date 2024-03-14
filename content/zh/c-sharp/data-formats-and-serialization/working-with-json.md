---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.654248-07:00
description: "\u4F7F\u7528 JSON (JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5) \u5305\
  \u62EC\u89E3\u6790\u3001\u751F\u6210\u548C\u67E5\u8BE2 JSON \u6570\u636E\uFF0C\u8FD9\
  \u662F\u73B0\u4EE3\u7F16\u7A0B\u7684\u4E00\u9879\u5173\u952E\u6280\u80FD\u3002\u8FD9\
  \u79CD\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u5728 Web \u670D\u52A1\u548C API \u4E2D\
  \u7684\u4F7F\u7528\u6781\u4E3A\u5E7F\u6CDB\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u9605\
  \u8BFB\u4E14\u4E0E\u8BED\u8A00\u65E0\u5173\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u5BF9\u4E8E\
  \u5904\u7406\u7F51\u7EDC\u5E94\u7528\u6216\u4E0E\u57FA\u4E8E Web \u7684\u6570\u636E\
  \u4EA4\u4E92\u7684 C# \u7A0B\u5E8F\u5458\u6765\u8BF4\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.791971-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 JSON (JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5) \u5305\u62EC\
  \u89E3\u6790\u3001\u751F\u6210\u548C\u67E5\u8BE2 JSON \u6570\u636E\uFF0C\u8FD9\u662F\
  \u73B0\u4EE3\u7F16\u7A0B\u7684\u4E00\u9879\u5173\u952E\u6280\u80FD\u3002\u8FD9\u79CD\
  \u6570\u636E\u4EA4\u6362\u683C\u5F0F\u5728 Web \u670D\u52A1\u548C API \u4E2D\u7684\
  \u4F7F\u7528\u6781\u4E3A\u5E7F\u6CDB\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u9605\u8BFB\
  \u4E14\u4E0E\u8BED\u8A00\u65E0\u5173\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u5BF9\u4E8E\u5904\
  \u7406\u7F51\u7EDC\u5E94\u7528\u6216\u4E0E\u57FA\u4E8E Web \u7684\u6570\u636E\u4EA4\
  \u4E92\u7684 C# \u7A0B\u5E8F\u5458\u6765\u8BF4\u81F3\u5173\u91CD\u8981\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用 JSON (JavaScript 对象表示法) 包括解析、生成和查询 JSON 数据，这是现代编程的一项关键技能。这种数据交换格式在 Web 服务和 API 中的使用极为广泛，因为它易于阅读且与语言无关，这使得它对于处理网络应用或与基于 Web 的数据交互的 C# 程序员来说至关重要。

## 如何操作：

### 将 JSON 字符串解析为对象

C# 提供了 `System.Text.Json` 命名空间来高效处理 JSON。要将 JSON 字符串解析为 C# 对象，定义一个与 JSON 结构匹配的类并使用 `JsonSerializer.Deserialize` 方法。

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 输出：Name: John, Age: 30
    }
}
```

### 从对象生成 JSON

要将 C# 对象转换回 JSON 字符串，请使用 `JsonSerializer.Serialize` 方法。

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // 输出：{"Name":"Jane","Age":25}
    }
}
```

### 使用 Newtonsoft.Json

`Newtonsoft.Json`（或 Json.NET）是一个流行的第三方库，为 JSON 的序列化和反序列化提供了更多的灵活性和选项。

要使用 Json.NET，首先必须通过 NuGet 安装 `Newtonsoft.Json` 包。然后，您可以这样反序列化 JSON 字符串：

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 输出：Name: Mike, Age: 22
    }
}
```

使用 Json.NET 从对象生成 JSON：

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // 输出：{"Name":"Ella","Age":28}
    }
}
```

这些代码片段为 C# 中的 JSON 处理提供了一个快速入门，演示了内置的 `System.Text.Json` 功能和 `Newtonsoft.Json` 的广泛功能。

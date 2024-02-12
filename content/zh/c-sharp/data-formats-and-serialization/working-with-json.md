---
title:                "使用JSON进行编程"
aliases: - /zh/c-sharp/working-with-json.md
date:                  2024-02-03T19:22:08.654248-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

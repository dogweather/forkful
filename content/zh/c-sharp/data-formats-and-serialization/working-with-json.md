---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.654248-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C# \u63D0\u4F9B\u4E86 `System.Text.Json`\
  \ \u547D\u540D\u7A7A\u95F4\u6765\u9AD8\u6548\u5904\u7406 JSON\u3002\u8981\u5C06\
  \ JSON \u5B57\u7B26\u4E32\u89E3\u6790\u4E3A C# \u5BF9\u8C61\uFF0C\u5B9A\u4E49\u4E00\
  \u4E2A\u4E0E JSON \u7ED3\u6784\u5339\u914D\u7684\u7C7B\u5E76\u4F7F\u7528 `JsonSerializer.Deserialize`\
  \ \u65B9\u6CD5\u3002"
lastmod: '2024-04-05T22:38:46.953645-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A C# \u63D0\u4F9B\u4E86 `System.Text.Json`\
  \ \u547D\u540D\u7A7A\u95F4\u6765\u9AD8\u6548\u5904\u7406 JSON\u3002\u8981\u5C06\
  \ JSON \u5B57\u7B26\u4E32\u89E3\u6790\u4E3A C# \u5BF9\u8C61\uFF0C\u5B9A\u4E49\u4E00\
  \u4E2A\u4E0E JSON \u7ED3\u6784\u5339\u914D\u7684\u7C7B\u5E76\u4F7F\u7528 `JsonSerializer.Deserialize`\
  \ \u65B9\u6CD5\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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

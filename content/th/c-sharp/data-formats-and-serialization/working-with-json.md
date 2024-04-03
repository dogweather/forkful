---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:41.680498-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.247373-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:


### การแยกวิเคราะห์สตริง JSON เป็นอ็อบเจกต์
C# มีชื่อเนมสเปซ `System.Text.Json` สำหรับการประมวลผล JSON อย่างมีประสิทธิภาพ เพื่อแยกวิเคราะห์สตริง JSON เป็นอ็อบเจกต์ C#, ให้กำหนดคลาสที่ตรงกับโครงสร้าง JSON และใช้เมธอด `JsonSerializer.Deserialize`

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
        // ผลลัพธ์: Name: John, Age: 30
    }
}
```

### การสร้างสตริง JSON จากอ็อบเจกต์
เพื่อแปลงอ็อบเจกต์ C# กลับเป็นสตริง JSON, ใช้เมธอด `JsonSerializer.Serialize`.

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
        // ผลลัพธ์: {"Name":"Jane","Age":25}
    }
}
```

### การใช้ Newtonsoft.Json
`Newtonsoft.Json` (หรือ Json.NET) เป็นไลบรารีของบุคคลที่สามที่ได้รับความนิยมซึ่งเสนอความยืดหยุ่นและตัวเลือกมากขึ้นสำหรับการอนุมานและการแปลงข้อมูล JSON

เพื่อใช้ Json.NET, คุณต้องติดตั้งแพ็กเกจ `Newtonsoft.Json` ผ่าน NuGet ก่อน จากนั้น, คุณสามารถแยกวิเคราะห์สตริง JSON ได้ดังนี้:

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
        // ผลลัพธ์: Name: Mike, Age: 22
    }
}
```

สำหรับการสร้าง JSON จากอ็อบเจกต์ด้วย Json.NET:

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
        // ผลลัพธ์: {"Name":"Ella","Age":28}
    }
}
```

ตัวอย่างเหล่านี้เสนอการเริ่มต้นอย่างรวดเร็วในการจัดการกับ JSON ใน C#, โดยแสดงทั้งความสามารถภายในของ `System.Text.Json` และคุณสมบัติอันกว้างขวางของ `Newtonsoft.Json`.

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:35.137587-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.215505-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:


### การจับคู่รูปแบบง่ายๆ
เพื่อตรวจสอบว่าสตริงมีรูปแบบที่ระบุหรือไม่ คุณสามารถใช้เมธอด `Regex.IsMatch` จากเนมสเปซ `System.Text.RegularExpressions` ได้

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // ผลลัพธ์: True
    }
}
```

### การสกัดข้อมูล
การสกัดข้อมูลจากสตริงโดยใช้กลุ่มใน regex สามารถทำได้ด้วยเมธอด `Regex.Match`

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // ผลลัพธ์: Year: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // ผลลัพธ์: Month: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // ผลลัพธ์: Day: 12
        }
    }
}
```

### การแทนที่ข้อความ
เมธอด `Regex.Replace` ช่วยให้คุณสามารถแทนที่ข้อความในสตริงที่ตรงกับรูปแบบที่ระบุได้

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // ผลลัพธ์: Visit Google!
    }
}
```

### การแยกสตริง
คุณสามารถแยกสตริงเป็นอาร์เรย์ตามรูปแบบ regex โดยใช้เมธอด `Regex.Split`

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // ผลลัพธ์: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### การใช้ไลบรารีของบุคคลที่สาม
ในขณะที่ .NET Framework ให้การสนับสนุนนิพจน์ปกติอย่างกว้างขวาง ยังมีไลบรารีของบุคคลที่สามเช่น `PCRE.NET` ที่เสนอนิพจน์ปกติที่เข้ากันได้กับ Perl (PCRE) ใน C# ซึ่งอาจเป็นประโยชน์หากคุณต้องการคุณลักษณะหรือไวยากรณ์จากเครื่องยนต์ regex ของ Perl ที่ไม่มีในการใช้งานของ .NET

ในการใช้ `PCRE.NET` คุณจะต้องติดตั้งแพ็คเกจ NuGet ก่อน แล้วคุณสามารถใช้มันได้คล้ายๆ กับการใช้คลาส regex ของ .NET เนทีฟ

```csharp
// ตัวอย่างการใช้ PCRE.NET ที่นี่
// หมายเหตุ: จินตนาการถึงตัวอย่างที่คล้ายกับตัวอย่างข้างต้น ที่แสดงคุณลักษณะเฉพาะของ PCRE.NET
```

เมื่อคุณผสานรวมไลบรารีของบุคคลที่สามสำหรับนิพจน์ปกติ โปรดปรึกษาเอกสารของพวกเขาเพื่อข้อมูลการใช้งานและความเข้ากันได้อย่างละเอียด

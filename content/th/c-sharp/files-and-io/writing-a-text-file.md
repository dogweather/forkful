---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:04.828638-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: C# \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E01\u0E31\
  \u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E40\
  \u0E19\u0E21\u0E2A\u0E40\u0E1B\u0E0B `System.IO` \u0E42\u0E14\u0E22\u0E43\u0E2B\u0E49\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.244475-06:00'
model: gpt-4-0125-preview
summary: "C# \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\
  \u0E34\u0E19\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1B\
  \u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\
  \u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E40\u0E19\u0E21\u0E2A\u0E40\u0E1B\u0E0B `System.IO`\
  \ \u0E42\u0E14\u0E22\u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\
  \u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\
  \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\n\n#."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีทำ:
C# ทำให้การดำเนินการกับไฟล์เป็นเรื่องง่ายขึ้นด้วยเนมสเปซ `System.IO` โดยให้วิธีการที่ตรงไปตรงมาในการเขียนไฟล์ข้อความ นี่คือวิธีการเขียนไฟล์ข้อความพื้นฐานและการเพิ่มข้อความเข้าไปในไฟล์ที่มีอยู่

### เขียนไฟล์ข้อความจากต้นฉบับ
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "สวัสดี, โลก!";

        // เขียนเนื้อหาลงในไฟล์ใหม่
        File.WriteAllText(filePath, content);

        Console.WriteLine("เขียนไฟล์เสร็จสิ้น");
    }
}
```
**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์เสร็จสิ้น
```

### เพิ่มข้อความเข้าไปในไฟล์ที่มีอยู่
หากคุณต้องการเพิ่มข้อความเข้าไปที่ปลายไฟล์ที่มีอยู่ คุณสามารถใช้ `File.AppendAllText` 

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nเพิ่มเนื้อหาเพิ่มเติม";

        // เพิ่มเนื้อหาเข้าไปในไฟล์
        File.AppendAllText(filePath, additionalContent);

        Console.WriteLine("เพิ่มเนื้อหาเสร็จสิ้น");
    }
}
```
**ผลลัพธ์ตัวอย่าง:**
```
เพิ่มเนื้อหาเสร็จสิ้น
```

### การใช้ไลบรารีของบุคคลที่สาม: `StreamWriter`
สำหรับการควบคุมการเขียนที่ละเอียดยิ่งขึ้น รวมถึงการล้างอัตโนมัติและการเลือกการเข้ารหัส ให้ใช้ `StreamWriter`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "นี่เป็นตัวอย่างโดยใช้ StreamWriter.";

        // ใช้ StreamWriter เพื่อเขียนลงในไฟล์
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }

        Console.WriteLine("เขียนไฟล์ด้วย StreamWriter เสร็จสิ้น");
    }
}
```
**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์ด้วย StreamWriter เสร็จสิ้น
```

แต่ละวิธีการเหล่านี้ตอบสนองความต้องการที่แตกต่างกัน: วิธีการ `File` โดยตรงสำหรับการดำเนินการที่รวดเร็ว และ `StreamWriter` สำหรับสถานการณ์การเขียนที่ซับซ้อนยิ่งขึ้น เลือกใช้ตามความต้องการเฉพาะของคุณ โดยพิจารณาจากปัจจัยเช่นประสิทธิภาพและขนาดไฟล์

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:28.768098-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\
  \u0E17\u0E35\u0E48\u0E21\u0E35\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2D\u0E22\
  \u0E39\u0E48\u0E20\u0E32\u0E22\u0E43\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E2B\u0E25\u0E14\u0E01\u0E32\u0E23\u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32, \u0E2D\u0E48\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.243530-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\
  \u0E17\u0E35\u0E48\u0E21\u0E35\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2D\u0E22\
  \u0E39\u0E48\u0E20\u0E32\u0E22\u0E43\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E2B\u0E25\u0E14\u0E01\u0E32\u0E23\u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32, \u0E2D\u0E48\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25, \u0E2B\u0E23\u0E37\u0E2D\u0E14\u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\
  \u0E22\u0E32\u0E01\u0E23\u0E17\u0E35\u0E48\u0E21\u0E35\u0E02\u0E19\u0E32\u0E14\u0E43\
  \u0E2B\u0E0D\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E40\u0E2B\u0E21\u0E32\
  \u0E30\u0E2A\u0E21\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\u0E02\u0E49\u0E21\u0E07\u0E27\
  \u0E14\u0E14\u0E49\u0E27\u0E22\u0E42\u0E04\u0E49\u0E14."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไรและทำไม?
การอ่านไฟล์ข้อความคือการดึงข้อมูลจากไฟล์ที่มีข้อความอยู่ภายใน โปรแกรมเมอร์ทำสิ่งนี้เพื่อโหลดการตั้งค่า, อ่านข้อมูล, หรือดึงทรัพยากรที่มีขนาดใหญ่หรือไม่เหมาะสมที่จะเข้มงวดด้วยโค้ด

## วิธีการ:
เรามาเริ่มกันเลย นี่คือวิธีการอ่านจากไฟล์ใน C# โดยใช้ `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        
        // อ่านข้อความทั้งหมด
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // อ่านบรรทัดเข้าไปในอาร์เรย์
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // อ่านด้วย StreamReader
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

ตัวอย่างผลลัพธ์:

```
สวัสดี, นี่คือไฟล์ข้อความ
มีหลายบรรทัด
แต่ละบรรทัดจะถูกอ่านแยกกัน
```

## ลงลึก
การอ่านไฟล์ข้อความดูเหมือนเรื่องง่ายใช่ไหม? แต่มีประวัติศาสตร์เล็กน้อยและบางเรื่องราวที่น่ารู้

ในอดีต, ไฟล์ข้อความมักจะเป็นวิธีหลักในการเก็บข้อมูลก่อนที่ฐานข้อมูลจะถูกใช้งานอย่างแพร่หลาย โปรแกรมเมอร์ต้องจัดการการเข้าถึงไฟล์, จัดรูปแบบข้อมูลอย่างถูกต้อง, และจัดการกับข้อผิดพลาด C# ได้พัฒนาขึ้นมากมายนับแต่นั้นมา ตอนนี้, `System.IO` คือประเภทสำหรับการดำเนินการไฟล์

คุณมีตัวเลือก:

- `File.ReadAllText` อ่านทั้งหมดในครั้งเดียว — ดีสำหรับไฟล์ที่มีขนาดเล็ก
- `File.ReadAllLines` ให้แต่ละบรรทัดเป็นองค์ประกอบของอาร์เรย์ — สะดวกสำหรับการประมวลผลบรรทัด
- `StreamReader` อ่านบรรทัดต่อบรรทัด ซึ่งประหยัดหน่วยความจำสำหรับไฟล์ขนาดใหญ่

แต่ละวิธีนั้นจะล็อกไฟล์ขณะที่มันถูกใช้งาน สิ่งนี้สำคัญในกรณีที่กระบวนการอื่นอาจพยายามเข้าถึงไฟล์

จำไว้, ต้องจัดการข้อยกเว้นเช่น `FileNotFoundException` หรือ `IOException` เมื่อทำงานกับไฟล์ คุณไม่ต้องการแอปซึมที่ถูกหยุดทำงานอย่างไม่คาดคิด

## ดูเพิ่มเติม
มีคำถามเพิ่มเติมหรือต้องการขยายความรู้ของคุณ? ตรวจสอบลิงก์เหล่านี้:

- [เอกสาร MSDN เกี่ยวกับคลาส File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [เอกสาร MSDN เกี่ยวกับคลาส StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [บทช่วยสอนเกี่ยวกับการจัดการข้อยกเว้น](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:25.506173-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E2D\u0E19\
  \u0E38\u0E0D\u0E32\u0E15\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \ C# \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2D\u0E34\u0E19\u0E1E\u0E38\
  \u0E15\u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E17\u0E35\u0E48\u0E43\
  \u0E2B\u0E49\u0E21\u0E32\u0E15\u0E2D\u0E19\u0E40\u0E1B\u0E34\u0E14\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.241692-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E2D\u0E19\
  \u0E38\u0E0D\u0E32\u0E15\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \ C# \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2D\u0E34\u0E19\u0E1E\u0E38\
  \u0E15\u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E17\u0E35\u0E48\u0E43\
  \u0E2B\u0E49\u0E21\u0E32\u0E15\u0E2D\u0E19\u0E40\u0E1B\u0E34\u0E14\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การอ่านอาร์กิวเมนต์บน command line อนุญาตให้โปรแกรม C# ประมวลผลอินพุตจากผู้ใช้ที่ให้มาตอนเปิดโปรแกรม โปรแกรมเมอร์ใช้วิธีนี้เพื่อปรับแต่งพฤติกรรมแอปโดยไม่ต้องเปลี่ยนแปลงโค้ด

## วิธีทำ:
นี่คือวิธีกลืนอาร์กิวเมนต์ command line:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("คุณได้ป้อนอาร์กิวเมนต์ต่อไปนี้:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

หากคุณรันโปรแกรมของคุณเช่นนี้: `yourapp.exe arg1 arg2 arg3`, คาดหวังผลลัพธ์:

```
คุณได้ป้อนอาร์กิวเมนต์ต่อไปนี้:
arg1
arg2
arg3
```

## ค้นพบลึกๆ
ประเพณีของอาร์กิวเมนต์บน command line ย้อนกลับไปถึงยุคเริ่มแรกของคอมพิวเตอร์, อนุญาตให้ซอฟต์แวร์รุ่นเก่ามีความยืดหยุ่น ใน C#, `args` เป็นอาร์เรย์ของสตริงใน `Main()` ที่ถืออาร์กิวเมนต์ที่ส่งมา มีทางเลือกอื่นไหม? แน่นอน, มีไลบรารี เช่น `CommandLineParser` ที่ช่วยเพิ่มความสามารถ แต่สำหรับงานหลายๆ อย่าง, `args` เป็นเพื่อนที่รวดเร็วและง่ายดาย

ในส่วนลึก, แอปฯ C# เริ่มต้นด้วย `Main()`. เมื่อคุณเรียกแอปของคุณจาก command line หรือสคริปต์, ระบบปฏิบัติการจะใส่อาร์กิวเมนต์ลงในอาร์เรย์และส่งมันไปยัง `Main()`. ง่ายๆ

มีแอปที่ซับซ้อนไหม? บางทีคุณอาจต้องการแยกวิเคราะห์ธง, ตัวเลือก และค่า? นี่คือที่ที่ไลบรารีโดดเด่นด้วยการควบคุมมากขึ้นและโค้ดสำเร็จรูปน้อยลงกว่าการแยก `args` แบบดิบๆ แต่สำหรับอินพุตง่ายๆ? `args` ทางไป

## ดูเพิ่ม
- [Microsoft Docs เกี่ยวกับ Main() และอาร์กิวเมนต์บน command-line](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [ไลบรารี CommandLineParser บน GitHub](https://github.com/commandlineparser/commandline)
- [การสนทนาใน Stack Overflow เกี่ยวกับวิธีการแยกวิเคราะห์อาร์กิวเมนต์บน command line ใน C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)

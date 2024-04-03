---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:21.350847-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: C# \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E14\u0E49\u0E32\u0E19\
  \u0E25\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49, \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E40\
  \u0E21\u0E18\u0E2D\u0E14 `string.Replace` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E25\
  \u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E04\u0E33."
lastmod: '2024-03-17T21:57:56.210326-06:00'
model: gpt-4-0125-preview
summary: "C# \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E32\u0E22 \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\
  , \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E40\u0E21\u0E18\u0E2D\u0E14 `string.Replace`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E04\u0E33."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
C# ทำให้การจัดการข้อความง่ายดาย ด้านล่างนี้, ลองดูเมธอด `string.Replace` เพื่อแลกเปลี่ยนคำ

```C#
using System;

public class Program
{
    public static void Main()
    {
        string phrase = "Hello, World!";
        string updatedPhrase = phrase.Replace("World", "C#");
        
        Console.WriteLine(updatedPhrase); // ผลลัพธ์: Hello, C#!
    }
}
```

ไม่ยากเลยใช่ไหม? แต่ถ้าเราต้องการที่จะไม่สนใจตัวพิมพ์เล็กพิมพ์ใหญ่ หรือแทนที่เฉพาะคำทั้งหมดเท่านั้น? Regex มาช่วยได้:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "Apples grow on trees. apple pies are tasty.";
        string pattern = "\\bapple\\b"; // \b คือขอบเขตของคำใน Regex
        string replacement = "Orange";
        
        string updatedPhrase = Regex.Replace(phrase, pattern, replacement, RegexOptions.IgnoreCase);

        Console.WriteLine(updatedPhrase); // ผลลัพธ์: Oranges grow on trees. Orange pies are tasty.
    }
}
```

## การศึกษาลึกล้ำ
ในอดีต, การจัดการสตริงเป็นเรื่องยุ่งยาก C เป็นทุกสิ่งที่เรามี, และมันหมายถึงการจัดการกับอาร์เรย์ของอักขระและการวนซ้ำด้วยมือ C# ให้ของขวัญกับเรา: การจัดการสตริงอย่างง่ายดาย

ถ้า `string.Replace` หรือ `Regex.Replace` ไม่ตอบโจทย์, เรายังมีตัวเลือกอื่นๆ สำหรับข้อความขนาดใหญ่หรือรูปแบบที่ซับซ้อน, พิจารณาเขียน parser ที่กำหนดเองหรือใช้ไลบรารีเช่น Antlr

Regex มีประสิทธิภาพในการจับคู่รูปแบบแต่อาจช้า เมื่อประสิทธิภาพเป็นสิ่งสำคัญและคุณพร้อมที่จะหาข้อมูลละเอียด, วัดและเปรียบเทียบกับ `StringBuilder` สำหรับการทดแทนขนาดใหญ่และซ้ำๆ

## ดูเพิ่มเติม
- Microsoft Docs เกี่ยวกับ [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace)
- คลาส [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) ของ .NET สำหรับรูปแบบที่ซับซ้อนยิ่งขึ้น
- ตรวจสอบ Antlr สำหรับการแยกวิเคราะห์ที่ซับซ้อน: [The ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/)

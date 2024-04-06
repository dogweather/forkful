---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:10.316999-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E15\u0E49\u0E2D\u0E07\
  \u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\
  \u0E1A\u0E32\u0E07\u0E15\u0E31\u0E27\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48?\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33\u0E43\
  \u0E19 C#."
lastmod: '2024-04-05T22:40:46.947104-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\
  \u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E1A\u0E32\u0E07\u0E15\u0E31\u0E27\u0E2B\u0E23\
  \u0E37\u0E2D\u0E44\u0E21\u0E48?"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
ต้องการกำจัดอักขระบางตัวหรือไม่? นี่คือวิธีทำใน C#:

```C#
using System;
using System.Text.RegularExpressions;

class PatternDeletion
{
    static void Main()
    {
        string originalText = "B4n4n4 P1zza!";
        string pattern = @"[0-9]+"; // ลบทุกตัวเลข
        
        string cleanedText = Regex.Replace(originalText, pattern, string.Empty);
        
        Console.WriteLine(cleanedText); // แสดงผล: Bnnn Pzza!
    }
}
```
ต้องการตัด 'a' ที่ตามด้วยตัวเลขหรือไม่? ดูสิ:

```C#
string targetedRemoval = "C4ndy C4ne";
string complexPattern = @"a[0-9]"; // มุ่งเน้นที่ 'a' ที่ตามด้วยตัวเลขใดๆ

string refinedText = Regex.Replace(targetedRemoval, complexPattern, string.Empty);

Console.WriteLine(refinedText); // แสดงผล: Cndy Cne
```

## ลงลึก
Regex (Regular Expressions) สนับสนุนการจับคู่รูปแบบ, ย้อนกลับไปสู่รากฐานทฤษฎีในยุค 1950s (ขอบคุณ, ทฤษฎีออโตมาตา!). ทางเลือกที่ไม่ใช่ regex รวมถึงการใช้ `String.Replace()` สำหรับการแทนที่ที่ง่ายกว่า, หรืออัลกอริธึมที่กำหนดเองหากประสิทธิภาพเป็นสิ่งสำคัญ (เนื่องจาก regex มี overhead บางประการ) ทางเลือกเหล่านี้ขาดความยืดหยุ่นและความแม่นยำซึ่งทำให้ regex เป็นตัวเลือกสำหรับรูปแบบที่ซับซ้อน ในการใช้งานการลบรูปแบบ, ต้องระมัดระวังธรรมชาติดาบสองคมของ regex – พวกเขามีประสิทธิภาพแต่อาจเข้าใจยากและช้าสำหรับข้อมูลขนาดใหญ่

## ดูเพิ่มเติม
- คู่มือ Regex ของ Microsoft: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- Regex101 (เพื่อทดสอบรูปแบบ regex): https://regex101.com/
- บทนำสู่ทฤษฎีออโตมาตา: https://en.wikipedia.org/wiki/Automata_theory

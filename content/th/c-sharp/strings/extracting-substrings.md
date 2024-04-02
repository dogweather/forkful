---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:55.499944-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\
  \u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\
  \u0E2D\u0E01\u0E21\u0E32 \u2014 \u0E1B\u0E23\u0E30\u0E21\u0E32\u0E13\u0E27\u0E48\
  \u0E32\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\u0E01\u0E0A\
  \u0E34\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E14\u0E02\u0E2D\u0E07\u0E40\u0E04\u0E49\u0E01\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C,\u2026"
lastmod: '2024-03-17T21:57:56.214529-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\
  \u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\
  \u0E2D\u0E01\u0E21\u0E32 \u2014 \u0E1B\u0E23\u0E30\u0E21\u0E32\u0E13\u0E27\u0E48\
  \u0E32\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\u0E01\u0E0A\
  \u0E34\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E14\u0E02\u0E2D\u0E07\u0E40\u0E04\u0E49\u0E01\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไร & ทำไม?
การสกัดส่วนย่อยของสตริงคือการแยกส่วนที่เจาะจงของสตริงออกมา — ประมาณว่าเหมือนการตักชิ้นโปรดของเค้กของคุณออกมา โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการ, วิเคราะห์, หรือตรวจสอบส่วนเล็ก ๆ ของสตริงที่ใหญ่กว่าโดยไม่ต้องดิ้นรนกับทั้งหมด

## วิธีการ:
C# ทำให้การดึงส่วนย่อยของสตริงออกจากสตริงเป็นเรื่องง่าย นี่คือการดูอย่างรวดเร็วว่าทำอย่างไรโดยใช้วิธีการ `Substring` และการสไลซ์สตริงด้วยตัวดำเนินการช่วง

```C#
string fullString = "Hello, World! Life is beautiful.";
// ใช้ Substring(startIndex, length)
string extracted1 = fullString.Substring(7, 5); // "World"

Console.WriteLine(extracted1); // ผลลัพธ์: World

// ใช้การสไลซ์สตริงด้วยตัวดำเนินการช่วง [..]
string extracted2 = fullString[13..24]; // "Life is beau"

Console.WriteLine(extracted2); // ผลลัพธ์: Life is beau
```

## การทำความเข้าใจลึกซึ้ง
ส่วนย่อยของสตริงไม่ใช่เทคนิคใหม่ พวกมันมีมานานในภาษาเช่น C และ Java อย่างไรก็ตาม, C# ได้ปรับปรุงกระบวนการด้วยวิธีการและคุณสมบัติที่ให้ความสำคัญกับความสามารถอ่านและความง่ายในการใช้งาน

ในอดีต, โปรแกรมเมอร์ใช้ลูปและการคำนวณดัชนีอย่างระมัดระวัง วิธีการ `Substring` ใน C# เป็นการอัพเกรดที่ดี มันทำงานอย่างตรงไปตรงมา—ให้มันดัชนีเริ่มต้นและ, ตัวเลือก, ความยาว, และมันจะทำการสไลซ์ให้คุณ

การแสดงไม่จบเพียงแค่นั้น ด้วย C# 8.0 และเวอร์ชันที่ตามมา, เราได้รับการแนะนำตัวดำเนินการช่วงเช่น `[..]` พวกเขาอนุญาตให้มีการแสดงความเป็นธรรมชาติในการสไลซ์, โดยเฉพาะอย่างยิ่งเมื่อใช้ดัชนีที่เกี่ยวข้องกับปลายท้ายของสตริง (แสดงโดยตัวดำเนินการ `^`)

ตัวเลือกอื่น ๆ สำหรับ `Substring` รวมถึงวิธีการเช่น `Split`, การดำเนินการ Regex, หรือการจัดการสตริงด้วย LINQ การเลือกขึ้นอยู่กับสถานการณ์—คุณอาจแบ่งบรรทัด CSV, Regex รูปแบบหนึ่ง, หรือเลือกส่วนย่อยด้วยการแสดง LINQ ที่ประณีต

จากมุมมองของการประยุกต์ใช้, สตริง C# ไม่สามารถเปลี่ยนแปลงได้ เมื่อคุณเอาส่วนย่อย, คุณไม่ได้เปลี่ยนแปลงต้นฉบับ แทนที่จะสร้างสตริงใหม่ที่แบ่งปันพื้นที่หน่วยความจำบางส่วนกับต้นฉบับ—จนกว่าคุณจะเปลี่ยนแปลงมัน, แล้วมันก็จะไปยังการจัดสรรหน่วยความจำของตัวเอง

## ดูเพิ่มเติม
หากคุณต้องการทำความเข้าใจลึกซึ้งหรือสำรวจหัวข้อที่เกี่ยวข้อง, นี่คือทรัพยากรบางส่วน:
- เอกสารทางการของ Microsoft เกี่ยวกับ `Substring`: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- เพิ่มเติมเกี่ยวกับตัวดำเนินการช่วงและดัชนีใน C#: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-8.0/ranges
- การจัดการสตริงด้วย LINQ: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/
- การใช้งาน Regular Expressions ใน C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:41.689286-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\
  \u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E48\
  \u0E2D\u0E17\u0E49\u0E32\u0E22\u0E01\u0E31\u0E19 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\
  \u0E1E\u0E23\u0E32\u0E30\u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\u0E40\
  \u0E23\u0E32\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E04\u0E33\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E31\u0E0D\u0E25\u0E31\u0E01\u0E29\u0E13\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E25\u0E35, \u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.217550-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\
  \u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E48\
  \u0E2D\u0E17\u0E49\u0E32\u0E22\u0E01\u0E31\u0E19 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\
  \u0E1E\u0E23\u0E32\u0E30\u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\u0E40\
  \u0E23\u0E32\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E04\u0E33\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E31\u0E0D\u0E25\u0E31\u0E01\u0E29\u0E13\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E25\u0E35, \u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E33\u0E19\
  \u0E27\u0E13\u0E04\u0E48\u0E32\u0E41\u0E1A\u0E1A\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\
  \u0E01\u0E43\u0E2B\u0E49\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E2D\u0E48\
  \u0E32\u0E19\u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีการ:
การต่อสตริงใน C# สามารถทำได้หลายวิธี:

ใช้งาน `+` operator:
```C#
string hello = "Hello";
string world = "World";
string concatenated = hello + ", " + world + "!";
Console.WriteLine(concatenated); // ผลลัพธ์: Hello, World!
```

ใช้งาน `String.Concat()` method:
```C#
string concatenated = String.Concat("Hello", ", ", "World", "!");
Console.WriteLine(concatenated); // ผลลัพธ์: Hello, World!
```

ใช้งาน `StringBuilder` เพื่อประสิทธิภาพในลูป:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(", ");
sb.Append("World");
sb.Append("!");
Console.WriteLine(sb.ToString()); // ผลลัพธ์: Hello, World!
```

ใช้งาน string interpolation (C# 6.0 และสูงกว่า):
```C#
string world = "World";
string concatenated = $"Hello, {world}!";
Console.WriteLine(concatenated); // ผลลัพธ์: Hello, World!
```

## ลงลึก
การต่อสตริงไม่ใช่เรื่องใหม่; มันเกิดขึ้นมาตั้งแต่ยุคเริ่มต้นของการเขียนโปรแกรม อย่างไรก็ตาม, วิธีการที่เราทำใน C# นั้นได้พัฒนาขึ้น ดั้งเดิม, `+` ถูกใช้งานอย่างแพร่หลาย, แต่อาจจะไม่มีประสิทธิภาพเสมอไป, เฉพาะในลูป, เพราะสตริงใน .NET เป็น immutable การดำเนินการ `+` ทุกครั้งสร้างสตริงใหม่, ซึ่งอาจนำไปสู่ปัญหาด้านประสิทธิภาพ

`String.Concat()` เป็นการเรียก method โดยตรงที่ก็ไม่เหมาะกับลูปเช่นกัน แต่เหมาะกับจำนวนสตริงเล็กน้อยที่รู้จัก

`StringBuilder` เป็นไปเพื่อสถานการณ์ลูปหรือเมื่อสร้างสตริงแบบค่อยๆเพิ่มขึ้น เบื้องหลัง, `StringBuilder` รักษาบัฟเฟอร์เพื่อรองรับการเพิ่มโดยไม่ต้องสร้างสตริงใหม่สำหรับการเพิ่มทุกครั้ง

String interpolation, แนะนำใน C# 6.0, ช่วยให้โค้ดอ่านง่ายขึ้นและรักษาได้ง่าย เป็นการแปลเป็น `String.Format()` เรียกในระหว่างการคอมไพล์ แต่เป็นการสะดวกต่อการมองและลดความเสี่ยงต่อข้อผิดพลาด

แต่ละวิธีมีจุดมุ่งหมายของตัวเอง: การต่อสตริงอย่างรวดเร็ว (`+`), การรวมสตริงไม่กี่ตัว (`String.Concat()`), การสร้างสตริงหนักๆ (`StringBuilder`), และสตริงที่มีรูปแบบสวยงาม (string interpolation).

## ดูเพิ่มเติม
- Microsoft Docs เกี่ยวกับการต่อสตริง: [การต่อสตริง](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Microsoft Docs เกี่ยวกับ `StringBuilder`: [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:09.314488-06:00
description: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E0A\u0E48\u0E27\
  \u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E42\u0E04\
  \u0E49\u0E14 C# \u0E41\u0E25\u0E30\u0E23\u0E31\u0E19\u0E41\u0E1A\u0E1A\u0E42\u0E15\
  \u0E49\u0E15\u0E2D\u0E1A\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\
  \u0E27\u0E14\u0E40\u0E23\u0E47\u0E27 \u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49 C#\u2026"
lastmod: '2024-03-17T21:57:56.227917-06:00'
model: gpt-4-0125-preview
summary: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E42\u0E04\u0E49\
  \u0E14 C# \u0E41\u0E25\u0E30\u0E23\u0E31\u0E19\u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27 \u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49 C#\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
---

{{< edit_this_page >}}

## อะไรและทำไม?
REPL หรือ Read-Eval-Print Loop ช่วยให้คุณพิมพ์โค้ด C# และรันแบบโต้ตอบได้ โปรแกรมเมอร์ใช้มันสำหรับทดลองอย่างรวดเร็ว การดีบัก หรือเรียนรู้ C# โดยไม่ต้องตั้งค่าโปรเจกต์เต็มรูปแบบ

## วิธีการ:
เริ่มใช้งาน REPL ในสภาพแวดล้อม C# ของคุณโดยใช้หน้าต่าง C# Interactive หรือรัน `dotnet-script` ในเทอร์มินัลของคุณ นี่คือตัวอย่างการใช้งาน:

```csharp
> var greeting = "Hello, REPL!";
> Console.WriteLine(greeting);
Hello, REPL!
> 
```

คุณได้รับการตอบกลับทันที ไม่มีขั้นตอนการคอมไพล์และรัน เขียนโค้ดแล้วเห็นผลลัพธ์

## การดำดิ่งลึก
REPL เดินทางมาจาก Lisp ถึงภาษาสมัยใหม่ ซึ่งเติบโตในภาษาดินามิกเช่น Python ด้วย C#, Roslyn ได้นำ REPL มาใกล้กับนักพัฒนามากขึ้น `csi` สำหรับ Roslyn และ `dotnet-script` สำหรับ .NET Core เป็นตัวเลือกที่แข็งแกร่ง การตัดที่ลึกกว่า: พวกเขาประเมินโค้ดทีละบรรทัด ไม่ใช่ทั้งหมดพร้อมกัน ซึ่งเป็นโมเดลการดำเนินการที่แตกต่างจากแอปพลิเคชั่น C# ทั่วไป สิ่งนี้ส่งผลต่อการเก็บสถานะที่คงอยู่ข้ามการปฏิบัติการและขอบเขตของตัวแปร

หน้าต่าง C# Interactive ของ Visual Studio เป็น REPL ที่ขับเคลื่อนโดย Roslyn มี Intellisense, การอ้างอิงหลายรายการ, และการสนับสนุนแพ็กเกจ NuGet ต่างจากการทดลองในบรรทัดคำสั่งในยุคแรกๆอย่างมาก

สำหรับภาษาอื่น, Python ใช้ `IDLE`, JavaScript มี REPL ของ Node.js, และ F# มาพร้อมกับ `F# Interactive` แต่ละตัวส่งเสริมลูปการตอบรับทันทีที่ไม่มีค่าสำหรับการทดสอบสนิปเพตโค้ดขนาดเล็กหรือการเข้าใจคุณสมบัติของภาษา

## ดูเพิ่มเติม
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:15.362498-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E21\u0E32\u0E21\u0E49\
  \u0E27\u0E19\u0E41\u0E02\u0E19\u0E40\u0E2A\u0E37\u0E49\u0E2D\u0E02\u0E36\u0E49\u0E19\
  \u0E41\u0E25\u0E30\u0E25\u0E38\u0E22\u0E42\u0E04\u0E49\u0E14\u0E01\u0E31\u0E19\u0E40\
  \u0E16\u0E2D\u0E30 \u0E2A\u0E21\u0E21\u0E15\u0E34\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\
  \u0E21\u0E35 .NET 6 \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E0A\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E39\u0E07\u0E01\u0E27\u0E48\u0E32 - \u0E19\
  \u0E31\u0E48\u0E19\u0E04\u0E37\u0E2D\u0E25\u0E48\u0E32\u0E2A\u0E38\u0E14\u0E13 \u0E40\
  \u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E40\u0E02\u0E35\u0E22\u0E19 \u0E04\u0E38\u0E13\
  \u0E08\u0E30\u0E43\u0E0A\u0E49 .NET CLI \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E40\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E19\u0E35\u0E49\u2026"
lastmod: '2024-04-05T21:54:01.898233-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E21\u0E49\u0E27\u0E19\u0E41\u0E02\u0E19\u0E40\u0E2A\u0E37\
  \u0E49\u0E2D\u0E02\u0E36\u0E49\u0E19\u0E41\u0E25\u0E30\u0E25\u0E38\u0E22\u0E42\u0E04\
  \u0E49\u0E14\u0E01\u0E31\u0E19\u0E40\u0E16\u0E2D\u0E30 \u0E2A\u0E21\u0E21\u0E15\u0E34\
  \u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35 .NET 6 \u0E2B\u0E23\u0E37\u0E2D\
  \u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E39\u0E07\
  \u0E01\u0E27\u0E48\u0E32 - \u0E19\u0E31\u0E48\u0E19\u0E04\u0E37\u0E2D\u0E25\u0E48\
  \u0E32\u0E2A\u0E38\u0E14\u0E13 \u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E40\u0E02\
  \u0E35\u0E22\u0E19 \u0E04\u0E38\u0E13\u0E08\u0E30\u0E43\u0E0A\u0E49 .NET CLI \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E19\u0E35\u0E49\
  \ \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E2D\u0E1E\u0E04\u0E2D\u0E19\u0E42\u0E0B\
  \u0E25\u0E43\u0E2B\u0E21\u0E48."
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## วิธีการ:
มาม้วนแขนเสื้อขึ้นและลุยโค้ดกันเถอะ สมมติว่าคุณมี .NET 6 หรือเวอร์ชันที่สูงกว่า - นั่นคือล่าสุดณ เวลาที่เขียน คุณจะใช้ .NET CLI สำหรับเรื่องนี้

สร้างแอพคอนโซลใหม่:
```C#
dotnet new console -o MyNewProject
```
เข้าไปในโฟลเดอร์โปรเจกต์ของคุณ:
```C#
cd MyNewProject
```
รัน Hello World ต้นแบบที่คุณสร้างใหม่:
```C#
dotnet run
```
คุณควรได้เห็น:
```
Hello, World!
```
โปรเจกต์ใหม่ของคุณเริ่มต้นได้แล้ว!

## ลงลึก
ในอดีต คุณอาจจะเปิด Visual Studio และคลิกผ่านวิซาร์ด ไม่ใช่อีกต่อไป - ตอนนี้ .NET CLI คือที่สุด มันรวดเร็วและไม่สันนิษฐานมากเกี่ยวกับสภาพแวดล้อมการพัฒนาของคุณ

มีทางเลือกอื่นหรือไม่? แน่นอน Visual Studio ยังอยู่ที่นั่นสำหรับประสบการณ์ GUI Rider และ Visual Studio Code ก็เป็นตัวเลือกที่ดีเช่นกัน แต่ CLI? มันเกี่ยวกับบรรยากาศการเขียนสคริปต์ที่เรียบง่ายและมีประสิทธิภาพ 

รายละเอียดการดำเนินการ? ไฟล์ `.csproj` ของคุณถือกุญแจสู่อาณาจักร มันเป็น XML แต่ไม่ต้องกังวล - มันค่อนข้างดูแลตัวเอง ที่นี่มีข้อมูลที่กระบวนการสร้างต้องการ - เฟรมเวิร์กเป้าหมาย ความเกี่ยวข้อง การอ้างอิงโปรเจกต์ สิ่งดีๆ ทั้งหมด

## ดูเพิ่มเติม
- [เอกสารอย่างเป็นทางการของ .NET CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [หน้าผลิตภัณฑ์ Visual Studio](https://visualstudio.microsoft.com/)
- [ภาพรวมของ .NET Project SDK](https://docs.microsoft.com/en-us/dotnet/core/project-sdk/overview)

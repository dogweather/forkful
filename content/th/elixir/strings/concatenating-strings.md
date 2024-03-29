---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:48.053729-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21 (Concatenating strings) \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E23\u0E27\
  \u0E21\u0E2A\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E23\u0E37\
  \u0E2D\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\u0E32\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E14\
  \u0E35\u0E22\u0E27.\u2026"
lastmod: '2024-03-17T21:57:55.842247-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21 (Concatenating strings) \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E23\u0E27\
  \u0E21\u0E2A\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E23\u0E37\
  \u0E2D\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\u0E32\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E14\
  \u0E35\u0E22\u0E27.\u2026"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การต่อข้อความ (Concatenating strings) เป็นการรวมสองข้อความหรือมากกว่าเข้าด้วยกันเพื่อสร้างเป็นข้อความเดียว. คุณอาจต้องการรวมข้อความสำหรับการสร้างข้อความถึงผู้ใช้, สร้างเส้นทางไฟล์, หรือสำหรับกระบวนการ Serialization ข้อมูล. นี่คือการดำเนินการพื้นฐานในภาษาโปรแกรมมิ่งทุกภาษา, รวมถึง Elixir, ที่ช่วยให้นักพัฒนาสร้างสตริงแบบไดนามิกได้อย่างง่ายดาย.

## วิธีการ:
ใน Elixir, คุณสามารถต่อข้อความได้หลายวิธีที่ตรงไปตรงมา มาดูวิธีที่พบบ่อยที่สุด:

1. การใช้ operator `<>`, ซึ่งเป็นวิธีที่ง่ายและตรงไปตรงมาที่สุดในการต่อข้อความ:

```elixir
name = "Jane"
greeting = "Hello, " <> name <> "!"
IO.puts greeting
# ผลลัพธ์: Hello, Jane!
```

2. การใช้การแทรกค่า (interpolation) เพื่อ syntax ที่ชัดเจนยิ่งขึ้น, โดยเฉพาะเมื่อคุณต้องการแทรกตัวแปรลงในสตริง:

```elixir
name = "John"
age = 28
introduction = "My name is #{name} and I am #{age} years old."
IO.puts introduction
# ผลลัพธ์: My name is John and I am 28 years old.
```

3. การต่อรายการของสตริงด้วยฟังก์ชัน `Enum.join/2`:

```elixir
parts = ["Elixir", " is", " awesome!"]
message = Enum.join(parts)
IO.puts message
# ผลลัพธ์: Elixir is awesome!
```

จำไว้ว่า, แต่ละวิธีมีบริบทที่มันโดดเด่น, ดังนั้นเลือกตามความต้องการของคุณ.

## ศึกษาเพิ่มเติม
การต่อข้อความใน Elixir, เช่นเดียวกับในภาษาฟังก์ชันมากมาย, ไม่ได้ปราศจากความละเอียดอ่อน. เนื่องจากลักษณะของ Elixir ที่เป็น immutable, ทุกครั้งที่คุณต่อข้อความ, คุณจริงๆ กำลังสร้างสตริงใหม่. สิ่งนี้อาจนำไปสู่ผลกระทบต่อประสิทธิภาพสำหรับการดำเนินการที่มีการวนซ้ำสูง, ซึ่งภาษาอื่นๆ เช่น C หรือ Java อาจจัดการได้อย่างมีประสิทธิภาพมากกว่าเนื่องจากมีสตริงที่เปลี่ยนแปลงหรือบัฟเฟอร์เฉพาะ.

จากประสบการณ์, นักพัฒนาได้คิดหากลยุทธ์ต่างๆ เพื่อจัดการการต่อข้อความอย่างมีประสิทธิภาพในภาษาฟังก์ชัน. ตัวอย่างเช่น, การใช้รายการเพื่อสะสมสตริงและดำเนินการต่อข้อความเฉพาะในช่วงสุดท้ายนั้นเป็นรูปแบบที่พบบ่อย. วิธีการนี้ใช้ประโยชน์จากวิธีการที่รายการถูกนำไปใช้ใน Erlang (ระบบรันไทม์สำหรับ Elixir) สำหรับการใช้หน่วยความจำที่มีประสิทธิภาพมากขึ้น.

Elixir มี `IOList` เป็นทางเลือก, ช่วยให้คุณสามารถสร้างจำนวนข้อความใหญ่โดยไม่ต้องมีสตริงระหว่างทางที่คุณจะได้รับจากการต่อข้อความซ้ำๆ. IOList เป็นรายการที่ซ้อนกันของสตริงหรือรหัสตัวอักษรที่ BEAM (เครื่องเสมือนของ Erlang) สามารถเขียนโดยตรงไปยังผลลัพธ์, เช่น ไฟล์หรือเครือข่าย, โดยไม่ต้องต่อกันก่อน.

```elixir
content = ["Header", "\n", "Body text", "\n", "Footer"]
:ok = File.write("example.txt", content)
```

ในตัวอย่างนี้, `content` เป็น IOList และเราเขียนไปยังไฟล์โดยตรง. การดำเนินการประเภทนี้จะน้อยความเข้าใจและมีประสิทธิภาพน้อยกว่าหากทำโดยการต่อข้อความซ้ำๆ เพื่อสร้างเนื้อหาไฟล์ทั้งหมดในหน่วยความจำก่อน.

การเข้าใจแนวคิดหลักและเครื่องมือเหล่านี้สามารถปรับปรุงประสิทธิภาพและประสิทธิผลของคุณได้อย่างมากเมื่อจัดการกับการดำเนินการสตริงใน Elixir.

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติมเกี่ยวกับสตริงและประสิทธิผลใน Elixir, ทรัพยากรต่อไปนี้จะเป็นประโยชน์:

- [คู่มืออย่างเป็นทางการของ Elixir เกี่ยวกับ Binaries, Strings, และ Charlists](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [คู่มือประสิทธิภาพของ Erlang](http://erlang.org/doc/efficiency_guide/listHandling.html) - แม้ว่าจะออกแบบมาสำหรับ Erlang, แต่ส่วนใหญ่นี้ก็ใช้กับ Elixir ได้เนื่องจากมีพื้นฐานมาจาก Erlang VM.

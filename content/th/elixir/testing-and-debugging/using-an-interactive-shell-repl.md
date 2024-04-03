---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:04.518091-06:00
description: "Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A, \u0E2B\
  \u0E23\u0E37\u0E2D REPL (Read-Eval-Print Loop), \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\
  \u0E49\u0E04\u0E38\u0E13\u0E25\u0E2D\u0E07\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E40\u0E25\u0E47\u0E01 \u0E46 \u0E19\u0E49\u0E2D\u0E22 \u0E46 \u0E44\u0E14\u0E49\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E31\u0E19\u0E17\u0E35\u0E17\u0E31\u0E19\u0E43\
  \u0E14 Elixir programmers \u0E43\u0E0A\u0E49 REPL \u0E17\u0E35\u0E48\u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 IEx\u2026"
lastmod: '2024-03-17T21:57:55.854117-06:00'
model: gpt-4-0125-preview
summary: "Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A, \u0E2B\u0E23\
  \u0E37\u0E2D REPL (Read-Eval-Print Loop), \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E04\u0E38\u0E13\u0E25\u0E2D\u0E07\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\u0E40\
  \u0E25\u0E47\u0E01 \u0E46 \u0E19\u0E49\u0E2D\u0E22 \u0E46 \u0E44\u0E14\u0E49\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E17\u0E31\u0E19\u0E17\u0E35\u0E17\u0E31\u0E19\u0E43\u0E14\
  \ Elixir programmers \u0E43\u0E0A\u0E49 REPL \u0E17\u0E35\u0E48\u0E40\u0E23\u0E35\
  \u0E22\u0E01\u0E27\u0E48\u0E32 IEx (Interactive Elixir), \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07, \u0E01\u0E32\u0E23\u0E41\
  \u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  , \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\
  \u0E49\u0E20\u0E32\u0E29\u0E32."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
เพื่อเปิดใช้งาน IEx, เปิด terminal ของคุณและพิมพ์ `iex` นี่คือตัวอย่างเล็กน้อย:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

ผลลัพธ์ควรแสดงการกำหนดค่าตัวแปร, ผลลัพธ์ของฟังก์ชัน, และฟังก์ชันไม่ระบุชื่อที่ทำงาน.

## ลงลึก
Shell IEx ได้รับการรวมอยู่ใน Elixir ตั้งแต่วันแรก ๆ José Valim, ผู้สร้างของ Elixir, ได้รับแรงบันดาลใจจาก shell แบบโต้ตอบของภาษาอื่น ๆ เช่น `python` ของ Python และ `irb` ของ Ruby แม้ว่า IEx จะมีคุณสมบัติหลายอย่างที่เหมือนกับเหล่านี้, แต่มันถูกสร้างมาเพื่อรองรับลักษณะการทำงานพร้อมกันของ Elixir และถูกรวมอย่างเต็มรูปแบบกับความสามารถของ Erlang VM

ทางเลือกอื่น ๆ ของ IEx ในระบบนิเวศ Erlang รวมถึง `erl`, shell ของ Erlang แต่ IEx มอบสภาพแวดล้อมที่เป็นมิตรกับ Elixir มากขึ้น, พร้อมด้วยคุณสมบัติเช่น การเสร็จสิ้นงานด้วยแท็บอย่างครบถ้วน, ประวัติ, และผู้ช่วย

REPL ของ IEx นั้นมากกว่าสนามเด็กเล่น; มันสามารถเชื่อมต่อกับระบบที่กำลังทำงานอยู่ได้อย่างราบรื่น นี่คือสิ่งสำคัญสำหรับการแก้ไขข้อผิดพลาดในแอปพลิเคชันสด การประยุกต์ใช้งานที่อยู่ภายใต้นั้นอาศัย BEAM (Erlang VM), รับประกันว่าคุณสมบัติเช่นการสลับโค้ดสดนั้นได้รับการสนับสนุนอย่างถูกต้องใน shell

## ดูเพิ่มเติม
ดูเพิ่มเติมสำหรับการอ่านเพิ่มเติมและทรัพยากร:

- [เอกสาร IEx ของ Elixir](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - The Elixir Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [เอกสาร `erl` ของ Erlang](http://erlang.org/doc/man/erl.html)
- [การเรียนรู้ Shell แบบโต้ตอบของ Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)

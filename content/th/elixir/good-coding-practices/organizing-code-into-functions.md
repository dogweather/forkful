---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:06.177527-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A\u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\u0E48\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E40\u0E02\u0E49\u0E32\u0E44\u0E27\u0E49\u0E43\u0E19\u0E1A\u0E25\
  \u0E47\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49\u0E0B\u0E49\u0E33\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.858237-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A\u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\u0E48\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E40\u0E02\u0E49\u0E32\u0E44\u0E27\u0E49\u0E43\u0E19\u0E1A\u0E25\
  \u0E47\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49\u0E0B\u0E49\u0E33\u0E44\u0E14\u0E49 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E1E\u0E34\u0E48\
  \u0E21\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E1A\u0E33\
  \u0E23\u0E38\u0E07\u0E23\u0E31\u0E01\u0E29\u0E32 \u0E25\u0E14\u0E01\u0E32\u0E23\u0E0B\
  \u0E49\u0E33\u0E0B\u0E49\u0E2D\u0E19 \u0E41\u0E25\u0E30\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\
  \u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
ลองสร้างฟังก์ชัน Elixir ง่ายๆ ในการทำให้คำมีตัวพิมพ์ใหญ่:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
ผลลัพธ์:
```
Hello Elixir World
```
ที่นี่ เราได้บรรจุโลจิกในการทำให้ตัวอักษรเป็นตัวใหญ่ไว้อย่างเรียบร้อยในฟังก์ชันที่เรียกว่า `capitalize_words`.

## ลงลึก
ใน Elixir และระบบนิเวศ VM ของ Erlang ฟังก์ชันเป็นสิ่งสำคัญอย่างยิ่ง โดยรับปรัชญาของการแยกปัญหาออกเป็นส่วนๆ ที่เล็กกว่า สามารถจัดการได้ และเป็นอิสระ ทางประวัติศาสตร์แล้ว วิธีการทำงานนี้มีรากฐานมาจากแคลคูลัสแลมบ์ดาและ Lisps ซึ่งสนับสนุนปรัชญาของการทำให้โค้ดเป็นข้อมูล

ทางเลือกในการจัดระเบียบโค้ดสามารถใช้มาโครหรือกระบวนการใน Elixir สำหรับงานที่ซ้ำ ๆ หรือคองเคอเรนต์ ตามลำดับ ในเรื่องของการใช้งาน Elixir ฟังก์ชันสามารถจัดการกับการจับคู่รูปแบบและรับอาร์กิวเมนต์ที่แตกต่างกัน (arity) ให้ความหลากหลาย

## ดูเพิ่มเติม
- [เอกสารประกอบอย่างเป็นทางการของ Elixir เกี่ยวกับฟังก์ชัน](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)

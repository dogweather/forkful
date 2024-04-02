---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:48.777691-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E08\u0E32\u0E01\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E2B\u0E25\u0E31\u0E01\u0E21\u0E32\u0E43\u0E0A\u0E49 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\
  \u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E41\u0E25\u0E30\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u2026"
lastmod: '2024-03-17T21:57:55.839309-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E08\u0E32\u0E01\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E2B\u0E25\u0E31\u0E01\u0E21\u0E32\u0E43\u0E0A\u0E49 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\
  \u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E41\u0E25\u0E30\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไรและทำไม?
การดึงข้อความย่อยหมายถึงการดึงส่วนที่เฉพาะเจาะจงจากข้อความหลักมาใช้ โปรแกรมเมอร์ทำสิ่งนี้เพื่อจัดการและวิเคราะห์ข้อมูลข้อความ หรือเพื่อแสดงเฉพาะข้อมูลที่เกี่ยวข้องกับผู้ใช้เท่านั้น

## วิธีการ:
ใน Elixir, คุณสามารถดึงข้อความย่อยโดยใช้โมดูล `String` นี่คือวิธีการ:

```elixir
str = "Hello, World!"

# การดึงข้อความย่อยโดยการระบุช่วง
substr = String.slice(str, 7, 5)
IO.puts(substr)  # => World

# การดึงข้อความย่อยไปจนถึงสิ้นสุดของข้อความ
substr_end = String.slice(str, 7)
IO.puts(substr_end)  # => World!

# การดึงตัวอักษรเดียว (ซึ่งก็ถือเป็นข้อความย่อยโดยเทคนิค)
char = String.at(str, 1)
IO.puts(char)  # => e
```

ตัวอย่างเหล่านี้แสดงการดึงข้อความโดยการระบุช่วงดัชนี, ไปจนถึงสิ้นสุดของข้อความ, และการจับตัวอักษรเดียว

## ลงลึก
วิธีการใช้งานข้อความของ Elixir ได้รับอิทธิพลมาจากมรดกของ Erlang โดยใช้ binaries สำหรับการจัดเก็บสตริง การดึงข้อความย่อยแตกต่างจากภาษาเช่น C ซึ่งใช้สตริงที่จบด้วย null ข้อความย่อยใน Elixir เป็น UTF-8 และบายนารี-เซฟ หมายความว่าพวกเขาเคารพขอบเขตของตัวละคร

ในอดีต, ภาษาโปรแกรมและระบบต่างๆ มีวิธีจัดการสตริงของตัวเอง ซึ่งบ่อยครั้งนำไปสู่ปัญหาเรื่องการสากลนำและการจัดการหน่วยความจำ Elixir ด้วยสตริงบายนารีให้วิธีการที่สากลและมีประสิทธิภาพสำหรับจัดการสตริง

วิธีการอื่นๆ ใน Elixir สำหรับการดึงข้อความย่อยที่เกินกว่า `String.slice` และ `String.at` ส่วนใหญ่เกี่ยวข้องกับการใช้งาน regex หรือการจับคู่รูปแบบสตริง ซึ่งทั้งสองสามารถมีพลังมากแต่ก็ซับซ้อนมากขึ้น

รายละเอียดการดำเนินการมีความสำคัญเพราะการดึงข้อความย่อยอาจเป็นการใช้ทรัพยากรมาก โดยเฉพาะเมื่อจัดการกับสตริงขนาดใหญ่หรือดำเนินการมากมายอย่างไม่ถูกต้อง ลักษณะภาพการทำงานแบบฟังก์ชันของ Elixir สนับสนุนการประมวลผลสตริงในลักษณะที่ใช้ประโยชน์จากการจับคู่รูปแบบและการเรียกกลับซ้ำๆ (Recursion) ซึ่งสามารถช่วยเรื่องประสิทธิภาพและความชัดเจนของโค้ด

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติมและเอกสารที่ละเอียดยิ่งขึ้น คุณสามารถเข้าชมลิ้งก์เหล่านี้ได้:

- เอกสารอย่างเป็นทางการของโมดูล `String` ของ Elixir: [hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- การทำความเข้าใจ binaries และสตริงใน Elixir: [elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- การมองของ Elixir School เกี่ยวกับสตริงและการจับคู่รูปแบบ: [elixirschool.com/en/lessons/basics/strings](https://elixirschool.com/en/lessons/basics/strings/) และ [elixirschool.com/en/lessons/basics/pattern-matching](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- การใช้ regular expressions ใน Elixir: [hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)

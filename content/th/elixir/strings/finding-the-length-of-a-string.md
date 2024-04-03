---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:50.705332-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E08\u0E33\u0E19\u0E27\u0E19\
  \u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E35\u0E48\u0E21\u0E31\u0E19\
  \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19\
  , \u0E1A\u0E31\u0E07\u0E04\u0E31\u0E1A\u0E43\u0E0A\u0E49\u0E02\u0E49\u0E2D\u0E08\
  \u0E33\u0E01\u0E31\u0E14,\u2026"
lastmod: '2024-03-17T21:57:55.841357-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E08\u0E33\u0E19\u0E27\u0E19\
  \u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E35\u0E48\u0E21\u0E31\u0E19\
  \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19\
  , \u0E1A\u0E31\u0E07\u0E04\u0E31\u0E1A\u0E43\u0E0A\u0E49\u0E02\u0E49\u0E2D\u0E08\
  \u0E33\u0E01\u0E31\u0E14, \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E41\u0E19\
  \u0E27\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\u0E01\u0E40\u0E2D\u0E32\u0E15\
  \u0E4C\u0E1E\u0E38\u0E15."
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 7
---

## วิธีการ:
ใน Elixir, คุณสามารถดูความยาวของสตริงได้ด้วยฟังก์ชัน `String.length/1` นี่คือวิธีการ:

```elixir
my_string = "Hello, World!"
length = String.length(my_string)
IO.puts(length)
```

ตัวอย่างผลลัพธ์:

```
13
```

## การศึกษาลึก
ภายใน, สตริงของ Elixir ถูกเข้ารหัสด้วย UTF-8 เป็นไบนารี แต่ละตัวอักษรสามารถเป็นจำนวนไบต์ตั้งแต่หนึ่งถึงสี่ไบต์ เมื่อเราเรียก `String.length/1`, เราไม่ได้นับไบต์เพียงอย่างเดียว; เรานับ Unicode graphemes, ซึ่งเป็นสิ่งที่เรามองเห็นเป็นตัวอักษร

ในอดีต, การดำเนินการความยาวสตริงในหลายภาษามุ่งเน้นไปที่ไบต์และไม่ได้คำนึงถึงตัวอักษรหลายไบต์อย่างเหมาะสม Elixir เริ่มต้นด้วยวิธีการที่ทันสมัยและเป็นมิตรกับ Unicode ตั้งแต่แรก

สำหรับทางเลือกอื่น ๆ, คุณอาจนับ graphemes ด้วยตนเองโดยใช้ recursion หรือด้วย loop แต่นั่นไม่จำเป็นและไม่มีประสิทธิภาพ `String.length/1` ได้รับการปรับให้เหมาะสมและเป็นภาษาอย่างเป็นทางการ

การดำเนินการของ Elixir ใช้ Erlang NIF (Native Implemented Function) สำหรับ `String.length/1`, ทำให้มันเร็วมาก การนับไบต์แทน graphemes ทำได้ด้วย `byte_size/1`, ซึ่งนับไบต์ที่แท้จริงของการแสดงรูปแบบไบนารีของสตริง—มีประโยชน์ในการดำเนินงานระดับต่ำที่การเข้ารหัสไม่สำคัญ

## ดูเพิ่มเติม
- [เอกสารของโมดูลสตริงของ Elixir](https://hexdocs.pm/elixir/String.html)
- [มาตรฐาน Unicode](http://www.unicode.org/standard/standard.html)

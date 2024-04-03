---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:14.509094-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 (String interpolation)\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E40\u0E02\u0E49\
  \u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.836598-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 (String interpolation)\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E40\u0E02\u0E49\
  \u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\u0E14\u0E49 \u0E21\
  \u0E31\u0E19\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E01\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E27\u0E38\u0E48\u0E19\u0E27\u0E32\u0E22\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## อะไร & ทำไม?
การใส่ตัวแปรหรือการคำนวณเข้าไปในสตริง (String interpolation) ช่วยให้คุณสามารถแทรกตัวแปรหรือการคำนวณเข้าไปในสตริงได้ มันมีประโยชน์สำหรับการสร้างสตริงได้อย่างเป็นไดนามิกโดยไม่ต้องวุ่นวายกับการต่อสตริง

## วิธีการ:
```elixir
name = "Josie"
age = 28

# การใส่ตัวแปรลงในสตริง
greeting = "Hello, #{name}! You are #{age} years old."
IO.puts greeting
```
ตัวอย่างผลลัพธ์:
```
Hello, Josie! You are 28 years old.
```
```elixir
# การใส่นิพจน์ลงในสตริง
IO.puts "In five years, #{name} will be #{age + 5} years old."
```
ตัวอย่างผลลัพธ์:
```
In five years, Josie will be 33 years old.
```

## ลงลึก
ในยุคแรกๆ คุณจะต้องใช้ `+` หรือ `,` ในการต่อสตริงเข้าด้วยกัน ซึ่งเป็นเรื่องที่ลำบาก ภาษาโปรแกรมมิ่งจึงเริ่มใช้การใส่ตัวแปรเพื่อการที่ง่ายขึ้นและมีการอ่านที่ง่ายขึ้น Elixir ซึ่งเป็นภาษาสมัยใหม่ก็รองรับคุณสมบัตินี้เช่นกัน

นี่คือสิ่งที่เกิดขึ้นเบื้องหลังกับ `"Hello, #{name}!"`: ระหว่างการคอมไพล์, Elixir จะเปลี่ยนสตริงเป็นการต่อสตริงของส่วนไบนารี, ซึ่งมีประสิทธิภาพเพราะไบนารีใน Elixir นั้นเป็น immutable

วิธีอื่นในการจัดการสตริงโดยไม่ใช้การใส่ตัวแปรใน Elixir อาจรวมถึงการใช้ `String.concat/2` หรือตัวดำเนินการ `<>` แต่วิธีเหล่านี้ก็มีความสะดวกน้อยกว่าสำหรับสตริงที่ซับซ้อน

ไวยากรณ์การใส่ตัวแปร `"#{...}"` สามารถรวมนิพจน์ Elixir ใด ๆ ที่จะถูกประมวลผลและจากนั้นแปลงเป็นสตริง สิ่งนี้เป็นไปได้เนื่องจาก Elixir มีการพิมพ์แบบไดนามิกและรองรับนิพจน์ในสตริงเป็นหลัก แต่จำไว้ว่า มันควรใช้สำหรับนิพจน์ที่ง่ายขึ้นเพื่อรักษาความสามารถในการอ่านได้

## ดูเพิ่มเติม
- เอกสารของโมดูล `String` ใน Elixir: https://hexdocs.pm/elixir/String.html
- คู่มือเกี่ยวกับประเภทข้อมูลไบนารีใน Elixir: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:20.168089-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\
  \u0E43\u0E2B\u0E21\u0E48 \u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\
  \u0E48\u0E07 `mix new`."
lastmod: '2024-03-17T21:57:55.853022-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E1B\
  \u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E43\u0E2B\u0E21\u0E48 \u0E43\u0E2B\u0E49\u0E43\
  \u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `mix new`."
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## วิธีการ:
เพื่อสร้างโปรเจกต์ใหม่ ให้ใช้คำสั่ง `mix new`:

```elixir
$ mix new my_app
```

คุณจะเห็นผลลัพธ์ประมาณนี้:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

เข้าไปในไดเร็กทอรีโปรเจกต์ใหม่ของคุณ:

```elixir
$ cd my_app
```

ตอนนี้คุณสามารถรันโปรเจกต์หรือทดสอบมันได้:

รันโปรเจกต์ของคุณ:

```elixir
$ iex -S mix
```
ทดสอบมัน:

```elixir
$ mix test
```

## ดำดิ่งลึกลงไป
เครื่องมือสร้างของ Elixir, Mix, มาจากความปรารถนาที่จะให้วิธีที่น่าเชื่อถือและเป็นหนึ่งเดียวในการสร้าง, กำหนดค่า, และจัดการโปรเจกต์ Mix ได้รับอิทธิพลจากเครื่องมือจากระบบนิเวศอื่นๆ เช่น Bundler และ Rake ของ Ruby Mix นำการจัดการการพึ่งพาและการอัตโนมัติงานเข้ากับเครื่องมือของ Elixir ทางเลือกในภาษาอื่นอาจเป็น npm สำหรับ Node.js หรือ Maven สำหรับ Java อย่างไรก็ตาม Mix ถูกปรับให้เข้ากับเวลาทำงานของ Elixir และรวมกับรูปแบบแบบอย่างของมัน คำสั่ง `mix new` สร้างโครงสร้างมาตรฐานพร้อมกับไดเรกทอรีและไฟล์ที่กำหนดไว้ล่วงหน้า เช่น ไฟล์กำหนดค่า, การกำหนดโมดูล, และชุดทดสอบ การปฏิบัติตามข้อบังคับเป็นสิ่งสำคัญใน Elixir; มันส่งเสริมความสอดคล้องและการอ่านโค้ดได้อย่างง่ายดายในโปรเจกต์ Elixir

## ดูเพิ่มเติม
- เอกสาร `mix` อย่างเป็นทางการ: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- คู่มือโปรเจกต์ของ Elixir School: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)

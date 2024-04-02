---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:35.185014-06:00
description: "Elixir \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E41\u0E1A\u0E1A\u0E01\u0E23\u0E32\u0E1F\
  \u0E34\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E1F\u0E2A\u0E17\
  \u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 `:debugger`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19,\u2026"
lastmod: '2024-03-17T21:57:55.857332-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E41\u0E1A\u0E1A\u0E01\u0E23\u0E32\u0E1F\
  \u0E34\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E1F\u0E2A\u0E17\
  \u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 `:debugger`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E14\u0E35\u0E1A\u0E31\
  \u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
Elixir มาพร้อมกับตัวดีบักแบบกราฟิกอินเทอร์เฟสที่ใช้งานได้ภายในและเรียกว่า `:debugger` เพื่อใช้งาน, คุณจำเป็นต้องเริ่มต้นและเชื่อมต่อไปยังกระบวนการที่กำลังทำงาน

ขั้นแรก, ตรวจสอบว่าคุณได้เริ่ม `:debugger` ภายในเซสชั่น `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

ตอนนี้, ตีความโมดูลโค้ดที่คุณต้องการดีบัก:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

คุณสามารถตั้งจุดหยุด (breakpoint):
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

แล้ว, เรียกใช้ฟังก์ชันของคุณเพื่อให้ตรงกับจุดหยุดและเดินผ่านโค้ดของคุณ:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debugger จะหยุดการเดินทางที่บรรทัดที่มีจุดหยุด
```

## ดูลึกลงไป
ก่อนที่ Elixir จะมี `:debugger`, Erlang ได้มอบตัวดีบักที่ Elixir ใช้; มันแข็งแกร่งและเก่งอย่างยิ่งในการจัดการกับกระบวนการแบบขนาน(concurrent processes), ซึ่งเป็นจุดเด่นของ Erlang VM (BEAM) ไม่เหมือนกับตัวดีบักอื่นๆ, `:debugger` ไม่อนุญาตให้เปลี่ยนแปลงตัวแปรได้ทันที, เนื่องจากธรรมชาติของข้อมูลใน Elixir ที่ไม่สามารถเปลี่ยนแปลงได้ ส่วนทางเลือกอื่นๆ, คุณมี `IEx.pry` ที่ให้คุณหยุดการทำงานและกระโดดเข้าไปใน REPL ที่จุดใดก็ตามในโค้ดของคุณ, ซึ่งอาจเป็นประโยชน์มาก

ในขณะที่ `:debugger` เหมาะสำหรับอินเทอร์เฟสแบบกราฟิก, บางคนอาจชอบใช้เครื่องมือ `:observer` ซึ่งให้บริการตรวจสอบกระบวนการและเมตริกของระบบ, แม้ว่าจะไม่ได้มุ่งเน้นไปที่การเดินผ่านโค้ดโดยตรงก็ตาม ชุมชนของ Elixir ยังมีส่วนร่วมในการสร้างเครื่องมือเช่น `visualixir` และ `rexbug`, ขยายระบบนิเวศของเครื่องมือดีบักไปเหนือกว่าค่าเริ่มต้น

## ดูเพิ่มเติม
- คู่มือเริ่มต้นใช้งาน Elixir อย่างเป็นทางการเกี่ยวกับการดีบัก: https://elixir-lang.org/getting-started/debugging.html
- เอกสารของ Erlang `:debugger`: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- การอภิปรายในฟอรัม Elixir เกี่ยวกับเทคนิคการดีบัก: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15

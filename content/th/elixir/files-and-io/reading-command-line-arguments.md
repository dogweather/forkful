---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:34.690866-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19\u0E04\u0E2D\u0E21\u0E21\u0E32\
  \u0E19\u0E14\u0E4C\u0E44\u0E25\u0E19\u0E4C\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E14\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07\u0E08\u0E32\u0E01\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\
  \u0E31\u0E25\u2014\u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32\u0E2B\u0E23\u0E37\u0E2D\u0E0A\u0E37\u0E48\u0E2D\u0E44\u0E1F\u0E25\
  \u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.868863-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19\u0E04\u0E2D\u0E21\u0E21\u0E32\
  \u0E19\u0E14\u0E4C\u0E44\u0E25\u0E19\u0E4C\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E14\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07\u0E08\u0E32\u0E01\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\
  \u0E31\u0E25\u2014\u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32\u0E2B\u0E23\u0E37\u0E2D\u0E0A\u0E37\u0E48\u0E2D\u0E44\u0E1F\u0E25\
  \u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\
  \u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\
  \u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E42\u0E14\u0E22\u0E44\
  \u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E42\
  \u0E04\u0E4A\u0E14."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีการ:
ใน Elixir, การดึงอาร์กิวเมนต์บนคอมมานด์ไลน์ง่ายมาก ใช้ `System.argv()` แล้วคุณจะได้รับพวกมันเป็นรายการของสตริง

```elixir
defmodule CliArgs do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CliArgs.main()
```

รันสิ่งนี้โดยใช้คำสั่ง `elixir cli_args.exs foo bar baz`, และคาดหวัง:

```
["foo", "bar", "baz"]
```

คุณกำลังเห็นอาร์กิวเมนต์ `foo`, `bar`, และ `baz` อยู่ในลิสต์ของ Elixir นั้นเอง

## ลงลึก
โดยประวัติศาสตร์แล้ว, อาร์กิวเมนต์บนคอมมานด์ไลน์เก่าแก่ดั่งเกล็ดเขา, มีต้นกำเนิดมาจากสภาพแวดล้อม CLI ตั้งแต่ยุคแรก ใน Elixir, `System.argv()` เป็นพาหนะที่คุณสามารถไว้วางใจได้สำหรับงานนี้ เพราะอะไร? เพราะมันถูกฝังอยู่ใน Erlang VM, ซึ่ง Elixir ถูกสร้างขึ้นมาบนนั้น

มีทางเลือกอื่นไหม? แน่นอน, คุณมีไลบรารีต่างๆ ที่ช่วยวิเคราะห์อาร์กิวเมนต์ให้คุณ, โดยเพิ่มธงและตัวเลือกต่างๆ แต่สำหรับ Elixir แบบวานิลลา, `System.argv()` คือทางที่จะไป

จากมุมมองการประยุกต์ใช้, มันสำคัญที่จะจำไว้ว่า `System.argv()` ให้คุณรับอาร์กิวเมนต์ทั้งหมดเป็นสตริง หากคุณต้องการตัวเลขหรือประเภทอื่นๆ, คุณจะต้องแปลงมันด้วยตัวเอง นอกจากนี้, ลำดับมีความสำคัญ อาร์กิวเมนต์คำสั่งแรกของคุณคือ `List.first(System.argv())`, และต่อไปเช่นกัน

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติม, ตรวจสอบ:
- [เอกสารโมดูล System ของ Elixir](https://hexdocs.pm/elixir/System.html) สำหรับฟังก์ชันที่มีประโยชน์อื่นๆ เกี่ยวกับระบบ
- [Optparse](https://hexdocs.pm/elixir/OptionParser.html) ในไลบรารีมาตรฐานของ Elixir, ซึ่งเป็นเครื่องมือสำคัญสำหรับการวิเคราะห์ตัวเลือกบนคอมมานด์ไลน์
- [เอกสาร init ของ Erlang](http://erlang.org/doc/man/init.html) หากคุณสนใจเกี่ยวกับเวทมนตร์ที่อยู่ในเครื่อง VM ที่สนับสนุน Elixir

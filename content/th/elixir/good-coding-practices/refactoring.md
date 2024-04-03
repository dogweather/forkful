---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:12.395794-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E08\
  \u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A Elixir \u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B \u0E42\u0E14\u0E22\u0E40\u0E23\
  \u0E32\u0E08\u0E30 Refactor \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `calculate_stats`\
  \ \u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\u0E32\u0E19\u0E21\u0E32\u0E01\u0E40\u0E01\
  \u0E34\u0E19\u0E44\u0E1B\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\
  \u0E21\u0E31\u0E19\u0E2D\u0E2D\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E0A\u0E34\u0E49\u0E19\
  \u0E40\u0E25\u0E47\u0E01 \u0E46 \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E0A\u0E49\u0E0B\u0E49\u0E33\u0E44\u0E14\u0E49."
lastmod: '2024-03-17T21:57:55.861642-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A Elixir \u0E17\u0E31\u0E48\u0E27\u0E44\
  \u0E1B \u0E42\u0E14\u0E22\u0E40\u0E23\u0E32\u0E08\u0E30 Refactor \u0E1F\u0E31\u0E07\
  \u0E01\u0E4C\u0E0A\u0E31\u0E19 `calculate_stats` \u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E21\u0E32\u0E01\u0E40\u0E01\u0E34\u0E19\u0E44\u0E1B\u0E42\u0E14\u0E22\
  \u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E21\u0E31\u0E19\u0E2D\u0E2D\u0E01\u0E40\
  \u0E1B\u0E47\u0E19\u0E0A\u0E34\u0E49\u0E19\u0E40\u0E25\u0E47\u0E01 \u0E46 \u0E17\
  \u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E0B\u0E49\u0E33\
  \u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
ลองจัดระเบียบรูปแบบ Elixir ทั่วไป โดยเราจะ Refactor ฟังก์ชัน `calculate_stats` ที่ทำงานมากเกินไปโดยการแบ่งมันออกเป็นชิ้นเล็ก ๆ ที่สามารถใช้ซ้ำได้

```elixir
defmodule Stats do
  # โค้ดเดิมที่ยังไม่ได้ Refactor
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # โค้ดที่ผ่านการ Refactor
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# ตัวอย่างผลลัพธ์
# ก่อน Refactoring
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# หลัง Refactoring
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
ดังที่คุณเห็น ผลลัพธ์ยังคงเดิม แต่ตอนนี้เรามีฟังก์ชันเฉพาะกิจที่สามารถใช้ซ้ำและทดสอบได้โดยอิสระ

## การดำดิ่งลึก
การ Refactor ไม่ใช่แนวคิดใหม่ มันเป็นส่วนสำคัญของการเขียนโปรแกรมตั้งแต่ยุคแรกของการพัฒนาซอฟต์แวร์ ผลงานที่น่าสนใจ เช่น "Refactoring: Improving the Design of Existing Code" โดย Martin Fowler ให้แนวปฏิบัติพื้นฐานสำหรับการ Refactor พร้อมข้อมูลเชิงลึกว่าเมื่อใดและวิธีการใช้งาน

ทางเลือกอื่นสำหรับการ Refactor ด้วยตนเอง รวมถึงเครื่องมือวิเคราะห์โค้ดอัตโนมัติ ซึ่งสามารถแนะนำหรือแม้กระทั่งดำเนินการ Refactor ได้ อย่างไรก็ตาม เครื่องมืออัตโนมัติอาจไม่เข้าใจบริบทเต็มรูปแบบของโค้ดได้ตลอดเวลาและอาจพลาดรายละเอียดที่ผู้ตรวจสอบมนุษย์จะค้นพบได้

รายละเอียดการดำเนินงานใน Elixir เฉพาะ รวมถึงการเข้าใจพาราดัมฟังก์ชันและการใช้การจับคู่รูปแบบ, บทบัญญัติป้องกัน, และตัวดำเนินการ pipe เพื่อเขียนโค้ดที่ชัดเจนและกระชับ ตัวอย่างเช่น การ Refactor มักจะเกี่ยวข้องกับการเปลี่ยนฟังก์ชันสไตล์ปฏิบัติการที่ซับซ้อนเป็นฟังก์ชันที่เล็กขึ้น องค์ประกอบได้ ซึ่งตามความชอบของ Elixir สำหรับความไม่เปลี่ยนแปลงและการดำเนินงานที่ไม่มีผลกระทบด้านข้าง

## ดูเพิ่มเติม
สำหรับเทคนิคการ Refactor ที่เฉพาะเจาะจงสำหรับ Elixir:
- [คู่มืออย่างเป็นทางการของ Elixir](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" โดย Martin Fowler](https://martinfowler.com/books/refactoring.html), สำหรับหลักการทั่วไปที่สามารถนำไปใช้กับ Elixir
- [Credo, เครื่องมือวิเคราะห์โค้ดในรูปแบบเติมเต็มสำหรับ Elixir](https://github.com/rrrene/credo) ที่ส encourages best practices.
- [เส้นทาง Elixir ใน Exercism](https://exercism.org/tracks/elixir), สำหรับการฝึกปฏิบัติที่มักจะเกี่ยวข้องกับการ Refactor

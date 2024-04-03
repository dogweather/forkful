---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:50.637173-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Ruby, \u0E04\
  \u0E38\u0E13\u0E2B\u0E48\u0E2D\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E2B\u0E23\u0E37\
  \u0E2D\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E14\
  \u0E49\u0E27\u0E22 `#{}` \u0E41\u0E25\u0E49\u0E27\u0E27\u0E32\u0E07\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E40\u0E04\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\
  \u0E01\u0E32\u0E28\u0E2A\u0E2D\u0E07\u0E2D\u0E31\u0E19 \u0E40\u0E0A\u0E48\u0E19."
lastmod: '2024-03-17T21:57:56.714054-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Ruby, \u0E04\u0E38\u0E13\u0E2B\u0E48\u0E2D\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E2B\u0E23\u0E37\u0E2D\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u0E14\u0E49\u0E27\u0E22 `#{}` \u0E41\u0E25\u0E49\u0E27\
  \u0E27\u0E32\u0E07\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2A\u0E2D\u0E07\u0E2D\u0E31\
  \u0E19 \u0E40\u0E0A\u0E48\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
ใน Ruby, คุณห่อตัวแปรหรือนิพจน์ของคุณด้วย `#{}` แล้ววางลงในสตริงที่มีเครื่องหมายอัญประกาศสองอัน เช่น:

```Ruby
name = "Jesse"
greeting = "Hey there, #{name}!"
puts greeting # => Hey there, Jesse!
```

คุณไม่ได้ถูกจำกัดเพียงแค่ตัวแปรเท่านั้น; โค้ด Ruby ใด ๆ ก็สามารถใส่ในนั้นได้:

```Ruby
price_per_kg = 5
quantity = 2
puts "Your total is: $#{price_per_kg * quantity}" # => Your total is: $10
```

จำไว้ว่า, อัญประกาศเดี่ยวไม่ได้ผล:

```Ruby
puts 'Hey there, #{name}!' # => Hey there, \#{name}!
```

## ดำดิ่งลึกลงไป
ในอดีต, เราคงต้องใช้ `+` หรือ `<<` ในการต่อสตริงและตัวแปรเข้าด้วยกัน ซึ่งทำให้รกไปหน่อย

```Ruby
email = "user" + "@" + "example.com"
```

เข้าสู่การแทนที่สตริงใน Ruby, วิธีที่เรียบง่ายกว่าในการผสมข้อความกับโค้ด Ruby ประเมินอะไรก็ตามที่อยู่ภายใน `#{}` และแปลงเป็นสตริงโดยอัตโนมัติ พิจารณาถึงงานที่มันช่วยประหยัดจากการแปลงและต่อสตริง:

```Ruby
"π ประมาณ #{Math::PI.round(2)}"
```

Ruby ไม่เพียงแต่เป็นภาษาเดียว; ภาษาอื่นๆ หลายภาษามีคุณสมบัติที่เอื้อมือมาช่วยนี้ในรูปแบบของตัวเอง แต่ระวัง: ไม่ใช่ภาษาอื่นๆ บางภาษา, Ruby จำกัดความสามารถนี้ไว้เฉพาะกับสตริงที่มีเครื่องหมายอัญประกาศคู่และกรณีพิเศษอื่นๆ (เช่น แบกติกและสัญลักษณ์) อัญประกาศเดี่ยวเพียงแค่ส่งออกสิ่งที่อยู่ข้างใน, ไม่ว่าจะเป็นวงเล็บหยิกหรือไม่ก็ตาม

## ดูเพิ่มเติม
- เอกสาร Ruby ว่าด้วยไวยากรณ์: [เอกสาร Ruby - ไวยากรณ์](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-Strings)
- การมองลงไปลึกและการจัดการสตริง: [Ruby-Doc.org - สตริง](https://ruby-doc.org/core-3.1.2/String.html)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:13.655998-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elixir \u0E44\u0E21\u0E48\
  \u0E44\u0E14\u0E49\u0E23\u0E27\u0E21\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C XML \u0E44\u0E27\u0E49\u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 \u0E01\u0E32\u0E23\u0E40\
  \u0E25\u0E37\u0E2D\u0E01\u0E43\u0E0A\u0E49 SweetXML \u0E40\u0E1B\u0E47\u0E19\u0E17\
  \u0E32\u0E07\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19."
lastmod: '2024-03-17T21:57:55.877563-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E23\u0E27\u0E21\u0E01\u0E32\
  \u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C XML \u0E44\u0E27\u0E49\
  \u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19 \u0E01\u0E32\u0E23\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E43\u0E0A\u0E49\
  \ SweetXML \u0E40\u0E1B\u0E47\u0E19\u0E17\u0E32\u0E07\u0E40\u0E25\u0E37\u0E2D\u0E01\
  \u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
Elixir ไม่ได้รวมการวิเคราะห์ XML ไว้ในไลบรารีมาตรฐาน การเลือกใช้ SweetXML เป็นทางเลือกที่นิยม นี่คือวิธีการใช้งาน:

```elixir
# เพิ่ม SweetXML ลงใน dependencies ใน mix.exs
{:sweet_xml, "~> 0.6"}

# ในโค้ดของคุณ
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# วิเคราะห์ XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # ผลลัพธ์: Tove
```

## การศึกษาลึก
XML, หรือภาษามาร์กอัพที่ขยายได้, มีมาตั้งแต่ปลายยุค 90 มันเป็นทั้งคำพูดมากมายแต่มีโครงสร้าง—เหมาะสำหรับการแลกเปลี่ยนข้อมูลที่ซับซ้อน ขณะที่ความนิยมของ JSON เพิ่มสูงขึ้นเนื่องจากความง่าย, XML ยังคงเป็นที่มั่นในระบบองค์กรและระบบทางการเงินหลายระบบเนื่องจากความสามารถในการแสดงออกและ schemas มาตรฐาน

ทางเลือกอื่น ๆ ได้แก่:
- JSON สำหรับการแลกเปลี่ยนข้อมูลที่เบาและมีคำพูดน้อย
- Protobuf หรือ Thrift สำหรับการสื่อสารข้อมูลที่ถูกซีเรียลไลซ์เป็นไบนารี โดยเฉพาะสำหรับระบบภายใน

ภายใต้ฝาปิด, ไลบรารี XML สำหรับ Elixir ใช้ไลบรารี :xmerl ของ Erlang สำหรับการวิเคราะห์ ซึ่งให้การสนับสนุนที่แข็งแกร่งแต่อาจจะมีความเข้าใจน้อยกว่าวิธีการทันสมัยมากขึ้น ในขณะที่ Elixir พัฒนาขึ้น, ไลบรารีที่ขับเคลื่อนด้วยชุมชนเช่น SweetXML หุ้มเส้นใยเหล่านี้ด้วยไวยากรณ์ที่ใกล้เคียงกับ Elixir มากขึ้น, ทำให้การจัดการ XML ง่ายขึ้น

## ดูเพิ่มเติมที่:
- SweetXML บน Hex: https://hex.pm/packages/sweet_xml
- มุมมองของ Elixir เกี่ยวกับการวิเคราะห์ XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- เอกสารต่างๆ เกี่ยวกับการจัดการ XML ฐานรากจาก xmerl: http://erlang.org/doc/apps/xmerl/index.html

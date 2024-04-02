---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:51.308381-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\
  \u0E32\u0E28\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\
  \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E04\u0E23\u0E2D\
  \u0E1A\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E44\u0E14\u0E49\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E20\u0E32\u0E22\u0E43\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E30\u0E2D\
  \u0E32\u0E14\u0E2A\u0E30\u0E2D\u0E49\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.838376-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\
  \u0E32\u0E28\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\
  \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E04\u0E23\u0E2D\
  \u0E1A\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E44\u0E14\u0E49\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E20\u0E32\u0E22\u0E43\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E30\u0E2D\
  \u0E32\u0E14\u0E2A\u0E30\u0E2D\u0E49\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## อะไรและทำไม?
การลบอัญประกาศออกจากสตริงหมายถึงการกำจัดเครื่องหมายครอบที่ไม่จำเป็นเพื่อให้ได้ข้อความภายในที่สะอาดสะอ้าน โปรแกรมเมอร์ทำเช่นนี้เพื่อทำความสะอาดข้อมูลนำเข้า, หลีกเลี่ยงข้อผิดพลาด และเตรียมข้อมูลสำหรับการประมวลผลที่อัญประกาศไม่ได้เป็นคุณสมบัติที่ต้องการ

## วิธีการ:
Elixir ไม่มีฟังก์ชั่น 'ลบอัญประกาศ' ในตัว แต่สามารถทำได้ง่ายด้วยการใช้การจับคู่รูปแบบหรือฟังก์ชั่น `String` ดูตัวอย่างต่อไปนี้:

```elixir
# ใช้การจับคู่รูปแบบ
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# การใช้งานตัวอย่าง
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# ใช้ String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# การใช้งานตัวอย่าง
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

ผลลัพธ์สำหรับทั้งสองวิธีจะเป็น:
```
"Hello, World!"
```

## ศึกษาลึกลงไป
ย้อนกลับไปในวันวาน, อัญประกาศในสตริงเป็นสิ่งที่ต้องระมัดระวัง - จัดการไม่ดีและบูม, ข้อผิดพลาดในไวยากรณ์หรือช่องโหว่ด้านความปลอดภัย Elixir ให้ความสำคัญกับการจับคู่รูปแบบทำให้คุณสามารถแยกและสร้างสตริงของคุณได้ด้วยความแม่นยำ โมดูล `String` ที่แข็งแกร่งมีประโยชน์ด้วยเช่นกัน, สามารถกำจัดอัญประกาศได้อย่างยืดหยุ่นด้วยฟังก์ชั่น `trim` ทางเลือกอื่น? นิพจน์ปกติสามารถถีบอัญประกาศออกไปได้, และไลบรารีภายนอกอาจมีพลังเพิ่มเติมหากคุณต้องการมากกว่าเพียงการลบที่พื้นฐาน

## ดูเพิ่มเติม
ลงลึกไปยังเหล่านี้:
- [โมดูล String ของ Elixir](https://hexdocs.pm/elixir/String.html)
- [เรียนรู้เพิ่มเติมเกี่ยวกับการจับคู่รูปแบบใน Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [นิพจน์ปกติใน Elixir (โมดูล Regex)](https://hexdocs.pm/elixir/Regex.html)

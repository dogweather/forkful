---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:47.041046-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 PowerShell,\
  \ \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E01\
  \u0E32\u0E23\u0E2D\u0E34\u0E19\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E42\u0E1E\u0E40\u0E25\
  \u0E17\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E21\
  \u0E35\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E04\u0E33\
  \u0E1E\u0E39\u0E14\u0E04\u0E39\u0E48\u0E41\u0E25\u0E30\u0E2A\u0E31\u0E0D\u0E25\u0E31\
  \u0E01\u0E29\u0E13\u0E4C `$` \u0E01\u0E48\u0E2D\u0E19\u0E0A\u0E37\u0E48\u0E2D\u0E15\
  \u0E31\u0E27\u0E41\u0E1B\u0E23 \u0E2B\u0E48\u0E2D\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\
  \u0E14\u0E49\u0E27\u0E22 `$()`\u2026"
lastmod: '2024-03-17T21:57:56.425392-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 PowerShell, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E0A\u0E49\u0E01\u0E32\u0E23\u0E2D\u0E34\u0E19\u0E40\u0E15\u0E2D\u0E23\
  \u0E4C\u0E42\u0E1E\u0E40\u0E25\u0E17\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E04\u0E39\u0E48\u0E41\u0E25\u0E30\
  \u0E2A\u0E31\u0E0D\u0E25\u0E31\u0E01\u0E29\u0E13\u0E4C `$` \u0E01\u0E48\u0E2D\u0E19\
  \u0E0A\u0E37\u0E48\u0E2D\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23 \u0E2B\u0E48\u0E2D\u0E19\
  \u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E14\u0E49\u0E27\u0E22 `$()` \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E1B\u0E23\u0E30\u0E40\u0E21\u0E34\u0E19\u0E04\u0E48\u0E32\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E31\u0E19\u0E17\u0E35."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
ใน PowerShell, คุณสามารถใช้การอินเตอร์โพเลทด้วยสตริงที่มีเครื่องหมายคำพูดคู่และสัญลักษณ์ `$` ก่อนชื่อตัวแปร ห่อนิพจน์ด้วย `$()` เพื่อประเมินค่าภายในสตริงทันที

```PowerShell
$name = "Alex"
$day = (Get-Date).DayOfWeek

# การแทรกตัวแปรพื้นฐาน
"Hello, $name! Happy $day!"

# การแทรกนิพจน์
"Pi to two decimal places is $(Math::Round([Math]::Pi, 2))"

# ผลลัพธ์
Hello, Alex! Happy Wednesday!
Pi to two decimal places is 3.14
```

## ลงลึก
PowerShell ได้นำการอินเตอร์โพเลทสตริงมาใช้โดยยืมแนวคิดมาจากภาษาโปรแกรมที่มีมาก่อนอย่าง Perl ก่อน PowerShell เวอร์ชั่น 3, เราใช้ตัวดำเนินการ `+` สำหรับการต่อสตริงหรือใช้ตัวดำเนินการรูปแบบ `-f` นี่คือวิวัฒนาการ:

- การต่อสตริงแบบเก่า: `"Hello, " + $name + "! It's " + $day + "."`
- ตัวดำเนินการรูปแบบ: `"Hello, {0}! It's {1}." -f $name, $day`

สตริงที่ถูกอินเตอร์โพเลทง่ายต่อการอ่านและมีโอกาสผิดพลาดน้อยลง ในเบื้องหลัง, PowerShell ตีความสตริงที่ถูกอินเตอร์โพเลทและแทนที่ตัวแปรหรือนิพจน์ด้วยค่าของมันเมื่อสตริงถูกประเมินค่า ไม่ใช่ตอนที่ถูกกำหนด

## ดูเพิ่มเติม
- [คำอธิบายตัวดำเนินการรูปแบบ](https://ss64.com/ps/syntax-f-operator.html)

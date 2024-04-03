---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:44.392440-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\
  \u0E22\u0E32\u0E27\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Fish."
lastmod: '2024-03-17T21:57:56.634838-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\u0E2D\u0E07\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Fish."
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 7
---

## วิธีทำ:
นี่คือวิธีการหาความยาวของสตริงใน Fish:

```Fish Shell
set my_string "Hello, World!"
echo (string length "$my_string")
```

ผลลัพธ์:

```
13
```

## ลงลึก
ใน Fish, ไม่เหมือนกับเชลล์อื่น ๆ, `string length` เป็นฟังก์ชันที่ติดตั้งมาพื้นฐานทำให้มันเป็นธรรมชาติและมีประสิทธิภาพสูง ในอดีต, เชลล์อื่น ๆ อาจจำเป็นต้องใช้รูปแบบไวยากรณ์ที่ยาวกว่าหรือเครื่องมือภายนอกเช่น `expr` หรือ `wc` Fish ทำให้งานง่ายขึ้นด้วยฟังก์ชั่นการจัดการสตริงที่แข็งแกร่งของมัน ซึ่ง `string length` ให้จำนวนตัวอักษร Unicode โดยตรง ซึ่งไม่เสมอกับจำนวนไบต์ โดยเฉพาะสำหรับตัวอักษรที่ไม่ใช่ ASCII

วิธีอื่น ๆ สำหรับการกำหนดความยาวสตริงในเชลล์ก่อนฟังก์ชัน `string` ใน Fish อาจจะไม่น่าเชื่อถือเพราะพวกมันไม่ได้ตระหนักถึงตัวอักษรที่มีหลายไบต์เสมอไป ทางด้านการปฏิบัติ `string length` นับ Unicode กราฟีม ซึ่งสำคัญสำหรับข้อความที่มีตัวอักษรที่รวมกันกับตัวอื่นๆเพื่อสร้างยูนิตที่เห็นได้เพียงหนึ่งเดียว

## ดูเพิ่มเติม
- เอกสารการใช้งาน Fish เกี่ยวกับการจัดการสตริง: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- มาตรฐาน Unicode สำหรับการเข้าใจกราฟีม: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)

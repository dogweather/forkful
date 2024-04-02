---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:47.535445-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E44\u0E2B\
  \u0E19\u0E21\u0E32\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E48\u0E32\
  \u0E07\u0E01\u0E31\u0E19\u0E40\u0E17\u0E48\u0E32\u0E44\u0E23\u0E43\u0E19\u0E40\u0E27\
  \u0E25\u0E32 \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E01\u0E32\u0E23\u0E01\u0E34\
  \u0E08\u0E01\u0E23\u0E23\u0E21, \u0E27\u0E31\u0E14\u0E0A\u0E48\u0E27\u0E07\u0E40\
  \u0E27\u0E25\u0E32,\u2026"
lastmod: '2024-03-17T21:57:55.774866-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E44\u0E2B\
  \u0E19\u0E21\u0E32\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E48\u0E32\
  \u0E07\u0E01\u0E31\u0E19\u0E40\u0E17\u0E48\u0E32\u0E44\u0E23\u0E43\u0E19\u0E40\u0E27\
  \u0E25\u0E32 \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E01\u0E32\u0E23\u0E01\u0E34\
  \u0E08\u0E01\u0E23\u0E23\u0E21, \u0E27\u0E31\u0E14\u0E0A\u0E48\u0E27\u0E07\u0E40\
  \u0E27\u0E25\u0E32,\u2026"
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## อะไรและทำไม?

การเปรียบเทียบวันที่สองวันหมายถึงการตรวจสอบว่าวันที่ไหนมาก่อนหรือห่างกันเท่าไรในเวลา เหล่าโปรแกรมเมอร์ทำการนี้เพื่อกำหนดการกิจกรรม, วัดช่วงเวลา, และจัดระเบียบข้อมูลตามลำดับเวลา

## วิธีการ:

ใน Python, คุณสามารถใช้โมดูล `datetime` เพื่อเปรียบเทียบวันที่ นี่คือวิธีการ:

```Python
from datetime import datetime

# กำหนดวันที่สองวัน
date_1 = datetime(2023, 3, 25)
date_2 = datetime(2023, 4, 1)

# เปรียบเทียบวันที่
print(date_1 < date_2)    # ผลลัพธ์: True
print(date_1 > date_2)    # ผลลัพธ์: False
print(date_1 == date_2)   # ผลลัพธ์: False

# คำนวณความแตกต่าง
difference = date_2 - date_1
print(difference.days)    # ผลลัพธ์: 7
```

## ลงลึกยิ่งขึ้น

การเปรียบเทียบวันที่ไม่ใช่เรื่องใหม่ มันเป็นกุญแจสำคัญในระบบที่เก่าแก่เท่ากับปฏิทินเอง Python `datetime` เพียงแค่ต่อยอดประเพณีนั้นในรูปแบบดิจิทัล เรามีวิธีอื่นๆ ในการเปรียบเทียบวันที่ เช่น การใช้ Unix timestamps, หรือห้องสมุดเช่น `dateutil` สำหรับงานที่ซับซ้อน แต่ `datetime` คือเครื่องมือหลัก มันเป็นตัวแทนของวันที่เป็นอ็อบเจกต์, อนุญาตให้มีการเปรียบเทียบโดยตรงโดยใช้ตัวดำเนินการเปรียบเทียบ (`<`, `>`, `==`, ฯลฯ) เมื่อคุณลบวันที่, คุณจะได้รับอ็อบเจกต์ `timedelta`, ที่บอกคุณความแตกต่างในวัน, วินาที, และไมโครวินาที

นอกจากนี้, เขตเวลาสามารถทำให้คุณเดินทางผิดพลาดได้ หากคุณกำลังจัดการวันที่ในเขตเวลาต่างๆ, คุณจะต้องทำให้มันเป็นอะแวร์ โปรแกรม Python มีห้องสมุด `pytz`, ซึ่งสามารถใช้ร่วมกับ `datetime` เพื่อจัดการเขตเวลาได้อย่างมีประสิทธิภาพ

## ดูเพิ่มเติม:

- เอกสารโมดูล Python `datetime`: [docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- สำหรับการจัดการเขตเวลา: [pytz](https://pypi.org/project/pytz/)
- ห้องสมุด `dateutil` สำหรับการจัดการวันที่ซับซ้อน: [dateutil](https://pypi.org/project/python-dateutil/)
- การเข้าใจ Unix Timestamps: [Unix Time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)

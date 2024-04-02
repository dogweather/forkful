---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:35.763341-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E08\u0E32\u0E01\u0E2D\u0E2D\u0E1A\u0E40\u0E08\u0E47\u0E01\u0E15\u0E4C\u0E40\u0E1B\
  \u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u2026"
lastmod: '2024-03-17T21:57:55.774045-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E08\u0E32\u0E01\u0E2D\u0E2D\u0E1A\u0E40\u0E08\u0E47\u0E01\u0E15\u0E4C\u0E40\u0E1B\
  \u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## อะไรและทำไม?
การแปลงวันที่เป็นสตริงทำให้วันที่เปลี่ยนจากออบเจ็กต์เป็นรูปแบบข้อความ โปรแกรมเมอร์ทำเช่นนี้เพื่อแสดงวันที่ให้เป็นมิตรต่อผู้ใช้หรือเพื่อเตรียมพวกมันสำหรับการเก็บไว้ในรูปแบบข้อความเช่น CSV หรือ JSON

## วิธีการ:
Python ทำให้การแปลงวันที่เป็นสตริงง่าย ใช้เมธอด `strftime` ที่มีให้บริการในวัตถุวันที่ นี่คือวิธีการ:

```Python
from datetime import datetime

# รับวันที่และเวลาปัจจุบัน
now = datetime.now()

# แปลงเป็นสตริงในรูปแบบ: Month day, Year
date_string = now.strftime("%B %d, %Y")
print(date_string)  # ผลลัพธ์: March 29, 2023 (หรือวันที่ปัจจุบัน)

# รูปแบบ: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # ผลลัพธ์: 2023-03-29 (หรือวันที่ปัจจุบัน)
```

## ลงลึก
ในอดีต การแปลงวันที่เป็นสตริงเป็นหนึ่งในหลักสูตรพื้นฐานในการเขียนโปรแกรม เนื่องจากความจำเป็นในการแสดงวันที่ในรูปแบบที่อ่านได้โดยมนุษย์

ทางเลือกสำหรับ `strftime` รวมถึงการใช้เมธอด `isoformat` สำหรับรูปแบบ ISO 8601 หรือไลบรารีของบุคคลที่สามเช่น `arrow` และ `dateutil` ที่เสนอตัวเลือกในการแยกวิเคราะห์และการจัดรูปแบบที่มีความยืดหยุ่นมากขึ้น

ในทางการทำงาน, `strftime` มีความหมายว่า "string format time" และมีรากฐานมาจากการเขียนโปรแกรมภาษา C Python ของ `strftime` แปลรหัสรูปแบบเช่น `%Y` สำหรับปีและ `%m` สำหรับเดือน ให้ความสามารถในการกำหนดรูปแบบได้อย่างไม่สิ้นสุด

## ดูเพิ่มเติม
เพื่อศึกษาลึกลงไปในฟังก์ชันวันและเวลาของ Python:
- เอกสาร `datetime` อย่างเป็นทางการของ Python: https://docs.python.org/3/library/datetime.html
- สำหรับผู้ที่สนใจรายการคำสั่ง `strftime` อย่างครบถ้วน: https://strftime.org/
- เพื่อสำรวจไลบรารีวัน/เวลาของบุคคลที่สาม:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/

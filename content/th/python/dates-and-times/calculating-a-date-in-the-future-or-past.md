---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:40.254842-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\
  \u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E0A\u0E48\u0E27\u0E07\u0E40\u0E27\
  \u0E25\u0E32\u0E17\u0E35\u0E48\u0E23\u0E30\u0E1A\u0E38\u0E44\u0E27\u0E49 \u0E40\u0E1B\
  \u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\u0E37\u0E2D\u0E19, \u0E27\
  \u0E31\u0E19\u0E2B\u0E21\u0E14\u0E2D\u0E32\u0E22\u0E38, \u0E01\u0E32\u0E23\u0E27\
  \u0E32\u0E07\u0E41\u0E1C\u0E19,\u2026"
lastmod: '2024-03-17T21:57:55.775833-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\
  \u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E0A\u0E48\u0E27\u0E07\u0E40\u0E27\
  \u0E25\u0E32\u0E17\u0E35\u0E48\u0E23\u0E30\u0E1A\u0E38\u0E44\u0E27\u0E49 \u0E40\u0E1B\
  \u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\u0E37\u0E2D\u0E19, \u0E27\
  \u0E31\u0E19\u0E2B\u0E21\u0E14\u0E2D\u0E32\u0E22\u0E38, \u0E01\u0E32\u0E23\u0E27\
  \u0E32\u0E07\u0E41\u0E1C\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## อะไรและทำไม?
การคำนวณวันที่ในอนาคตหรืออดีตหมายถึงการหาวันที่ก่อนหรือหลังจากช่วงเวลาที่ระบุไว้ เป็นสิ่งที่โปรแกรมเมอร์ทำเพื่อการแจ้งเตือน, วันหมดอายุ, การวางแผน, หรือการคำนวณที่อิงตามเวลา

## วิธีการ:
โมดูล `datetime` ของ Python ทำให้การทำงานกับวันที่และเวลาเป็นเรื่องง่าย ลองดูนี่:

```Python
from datetime import datetime, timedelta

# วันที่และเวลาปัจจุบัน
now = datetime.now()
print("ตอนนี้: ", now)

# เพิ่ม 10 วัน
future_date = now + timedelta(days=10)
print("วันที่ในอนาคต (+10 วัน): ", future_date)

# ลบ 5 วัน
past_date = now - timedelta(days=5)
print("วันที่ในอดีต (-5 วัน): ", past_date)
```
ผลที่อาจเป็นได้คือ:
```
ตอนนี้: 2023-04-01 12:34:56.789012
วันที่ในอนาคต (+10 วัน): 2023-04-11 12:34:56.789012
วันที่ในอดีต (-5 วัน): 2023-03-27 12:34:56.789012
```

ง่ายใช่ไหม? เพียงแค่ปรับจำนวนวัน, หรือใช้ `weeks`, `hours`, `minutes`, หรือ `seconds` ใน `timedelta` เพื่อกระโดดไปยังเวลาที่คุณต้องการ

## การศึกษาลึก
ก่อนหน้านี้, การคำนวณวันที่และเวลาเป็นเรื่องยาก คุณจะต้องจัดการกับปีอธิกสุรทิน, โซนเวลา, การปรับเวลาฤดูร้อน - รกสุดๆ ด้วยโมดูล `datetime` และเพื่อนๆ อย่าง `date` และ `time` ของ Python, มันเป็นเรื่องง่ายมาก โมดูลจัดการความซับซ้อนในเบื้องหลัง

คุณอาจถามเกี่ยวกับทางเลือกอื่น แน่นอน ไลบรารีเช่น `dateutil` สามารถจัดการกับการคำนวณวันที่ที่ซับซ้อนและการแยกวันที่ออกจากกัน มันเป็นตัวเลือกเมื่อ `datetime` อาจไม่ตอบโจทย์

ในเรื่องการใช้, เมื่อคุณใช้ `timedelta`, Python ปรับวันที่โดยคำนึงถึงปีอธิกสุรทินและอื่นๆ อย่างไรก็ตาม คุณควรตรวจสอบผลลัพธ์ของคุณ - โดยเฉพาะเมื่อต้องจัดการกับโซนเวลา และจำไว้ว่า, `datetime` เป็นแบบ naive โดยค่าเริ่มต้น; มันไม่คำนึงถึงโซนเวลาเว้นแต่คุณจะบอกมัน

## ดูเพิ่มเติม
- เอกสารแนะนำ `datetime` ของ Python: https://docs.python.org/3/library/datetime.html
- ไลบรารี `dateutil`: https://dateutil.readthedocs.io/en/stable/
- การจัดการกับโซนเวลาใน Python: https://docs.python.org/3/library/zoneinfo.html

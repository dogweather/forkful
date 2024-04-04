---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Python \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E02\u0E36\u0E49\u0E19 \u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\u2026"
lastmod: '2024-04-04T02:03:04.259754-06:00'
model: gpt-4-0125-preview
summary: "Python \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19 \u0E43\u0E0A\u0E49\u0E40\
  \u0E21\u0E18\u0E2D\u0E14 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\
  \ \u0E17\u0E35\u0E48\u0E21\u0E35\u0E43\u0E2B\u0E49\u0E1A\u0E23\u0E34\u0E01\u0E32\
  \u0E23\u0E1A\u0E19[\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  ](https://docs.python.org/3/library/datetime.html#date-objects) \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
Python ทำให้การแปลงวันที่เป็นสตริงง่ายขึ้น ใช้เมธอด [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) ที่มีให้บริการบน[วัตถุวันที่](https://docs.python.org/3/library/datetime.html#date-objects) นี่คือวิธีการ:

```Python
from datetime import datetime

# รับวันที่และเวลาปัจจุบัน
now = datetime.now()

# แปลงเป็นสตริงในรูปแบบ: เดือน วัน, ปี
date_string = now.strftime("%B %d, %Y")
print(date_string)  # ผลลัพธ์: มีนาคม 29, 2023 (หรือวันที่ปัจจุบัน)

# รูปแบบ: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # ผลลัพธ์: 2023-03-29 (หรือวันที่ปัจจุบัน)
```

### วิธีที่ฉันทำ

นี่คือวิธีที่ฉันได้รับวันที่ในรูปแบบ [ISO 8601](https://www.w3.org/QA/Tips/iso-date) พร้อมข้อมูลเขตเวลา:

```python
def datestamp() -> str:
    """ 
    วันที่และเวลาปัจจุบันพร้อมเขตเวลาในรูปแบบ ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### ตัวอย่างผลลัพธ์:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```

## ดูข้อมูลเพิ่มเติม
ประวัติศาสตร์ของการแปลงวันที่ไปเป็นสตริงเป็นสิ่งสำคัญในการเขียนโปรแกรมเนื่องจากความต้องการนำเสนอวันที่ในรูปแบบที่เป็นมิตรกับมนุษย์

ทางเลือกอื่นสำหรับ `strftime` รวมถึงการใช้เมธอด `isoformat` สำหรับรูปแบบ ISO 8601, หรือไลบรารี่ของบุคคลที่สามเช่น `arrow` และ `dateutil` ซึ่งเสนอตัวเลือกการแยกวิเคราะห์และการจัดรูปแบบที่ยืดหยุ่นมากขึ้น

ในแง่ของการประยุกต์ใช้, `strftime` มีความหมายว่า "รูปแบบสตริงเวลา" และมีรากฐานมาจากการเขียนโปรแกรมภาษา C Python ใช้ `strftime` ในการตีความรหัสรูปแบบเช่น `%Y` สำหรับปีและ `%m` สำหรับเดือน ซึ่งช่วยให้สามารถปรับแต่งได้อย่างไม่มีที่สิ้นสุด

## ดูเพิ่มเติม
หากต้องการศึกษาเพิ่มเติมเกี่ยวกับฟังก์ชันวันที่และเวลาของ Python:
- เอกสารการอ้างอิงอย่างเป็นทางการของ `datetime` ของ Python: https://docs.python.org/3/library/datetime.html
- สำหรับผู้ที่สนใจรายการคำสั่ง `strftime` แบบครบถ้วน: https://strftime.org/
- เพื่อสำรวจไลบรารี่วัน/เวลาของบุคคลที่สาม:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/

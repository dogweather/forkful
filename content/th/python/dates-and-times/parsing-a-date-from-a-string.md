---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:02.988906-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E21\u0E35\u0E2B\u0E19\u0E49\u0E32\u0E17\u0E35\u0E48\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\
  \u0E47\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\
  \u0E27\u0E25\u0E32 (datetime object) \u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E48\u0E32\u2026"
lastmod: '2024-03-17T21:57:55.772062-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E21\u0E35\u0E2B\u0E19\u0E49\u0E32\u0E17\u0E35\u0E48\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\
  \u0E47\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\
  \u0E27\u0E25\u0E32 (datetime object) \u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E48\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแยกวิเคราะห์วันที่จากสตริงมีหน้าที่ในการแปลงข้อมูลวันที่และเวลาที่เป็นข้อความเป็นวัตถุวันที่เวลา (datetime object) หรือรูปแบบที่มีโครงสร้างเทียบเท่า โดยทั่วไปจะทำเพื่ออำนวยความสะดวกในการคำนวณวันที่, การเปรียบเทียบ, และการจัดรูปแบบการดำเนินการในลักษณะที่ไม่ขึ้นกับภาษาและภูมิภาค โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการและควบคุมข้อมูลเวลาที่สกัดจากบันทึก, ข้อมูลที่ป้อนโดยผู้ใช้, หรือแหล่งภายนอกอย่างมีประสิทธิภาพ

## วิธีการ:
ไลบรารีมาตรฐานของ Python มีโมดูล `datetime` ซึ่งรวมถึงวิธีการ `strptime` สำหรับวัตถุประสงค์นี้ วิธีการนี้ต้องการอาร์กิวเมนต์สองอย่าง: สตริงของวันที่ และคำสั่งรูปแบบที่ระบุรูปแบบของสตริงที่ป้อน

```python
from datetime import datetime

# สตริงตัวอย่าง
date_string = "2023-04-01 14:30:00"
# การแยกวิเคราะห์สตริงเป็นวัตถุวันที่เวลา
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# ผลลัพธ์: 2023-04-01 14:30:00
```

สำหรับการแยกวิเคราะห์วันที่ที่ซับซ้อนยิ่งกว่า, โดยเฉพาะเมื่อจัดการกับหลายรูปแบบหรือสถานที่, ไลบรารีภายนอก `dateutil` สามารถช่วยได้มาก มันเสนอโมดูล parser ซึ่งสามารถแยกวิเคราะห์วันที่ในรูปแบบสตริงได้แทบทุกรูปแบบ

```python
from dateutil import parser

# สตริงตัวอย่าง
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# ใช้ parser ของ dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# ผลลัพธ์: 2023-04-01 14:30:00
print(parsed_date2)
# ผลลัพธ์: 2023-04-01 14:30:00
```

`dateutil` มีความชำนาญในการจัดการกับรูปแบบวันที่โดยไม่ต้องใช้สตริงรูปแบบอย่างชัดเจน, ทำให้เป็นตัวเลือกที่หลากหลายสำหรับการใช้งานที่มีการจัดการกับการแสดงวันที่ที่หลากหลาย

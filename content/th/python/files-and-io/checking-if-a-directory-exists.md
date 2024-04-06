---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:32.731864-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Python \u0E21\u0E35\u0E27\
  \u0E34\u0E18\u0E35\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E42\u0E1F\u0E25\u0E40\u0E14\u0E2D\u0E23\u0E4C\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `os` \u0E41\u0E25\
  \u0E30 `pathlib` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E17\u0E31\u0E49\u0E07\u0E2A\
  \u0E2D\u0E07."
lastmod: '2024-03-17T21:57:55.776787-06:00'
model: gpt-4-0125-preview
summary: "Python \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E42\u0E1F\
  \u0E25\u0E40\u0E14\u0E2D\u0E23\u0E4C\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E21\
  \u0E14\u0E39\u0E25 `os` \u0E41\u0E25\u0E30 `pathlib` \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E17\u0E31\u0E49\u0E07\u0E2A\u0E2D\u0E07."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
Python มีวิธีพื้นฐานในการตรวจสอบการมีอยู่ของโฟลเดอร์โดยใช้โมดูล `os` และ `pathlib` นี่คือตัวอย่างสำหรับทั้งสอง:

### การใช้งานโมดูล `os`
```python
import os

# ระบุที่อยู่ของโฟลเดอร์
dir_path = "/path/to/directory"

# ตรวจสอบว่าโฟลเดอร์มีอยู่
if os.path.isdir(dir_path):
    print(f"โฟลเดอร์ {dir_path} มีอยู่.")
else:
    print(f"โฟลเดอร์ {dir_path} ไม่มีอยู่.")
```

### การใช้งานโมดูล `pathlib`
```python
from pathlib import Path

# ระบุที่อยู่ของโฟลเดอร์
dir_path = Path("/path/to/directory")

# ตรวจสอบว่าโฟลเดอร์มีอยู่
if dir_path.is_dir():
    print(f"โฟลเดอร์ {dir_path} มีอยู่.")
else:
    print(f"โฟลเดอร์ {dir_path} ไม่มีอยู่.")
```

### ไลบรารีของบุคคลที่สาม
แม้ว่าไลบรารีมาตรฐานของ Python จะเพียงพอสำหรับการตรวจสอบว่ามีโฟลเดอร์อยู่หรือไม่, ไลบรารีเช่น `pathlib2` สามารถเป็นทางเลือกสำหรับความสอดคล้องกันในเวอร์ชัน Python ต่างๆ หรือฟังก์ชันเพิ่มเติม

***หมายเหตุ:*** ณ รุ่นล่าสุดของ Python, `pathlib` มีความเข้มแข็งเพียงพอสำหรับกรณีการใช้งานส่วนใหญ่ ทำให้ไลบรารีของบุคคลที่สามไม่จำเป็นสำหรับงานนี้โดยเฉพาะ

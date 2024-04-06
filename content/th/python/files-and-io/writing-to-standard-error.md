---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:29.710472-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E42\u0E21\u0E14\u0E39\
  \u0E25 `sys` \u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E15\
  \u0E31\u0E27\u0E02\u0E2D\u0E07 Python \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\
  \u0E31\u0E07 `stderr` \u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E0A\u0E31\
  \u0E14\u0E40\u0E08\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.778776-06:00'
model: gpt-4-0125-preview
summary: "\u0E42\u0E21\u0E14\u0E39\u0E25 `sys` \u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48\u0E43\u0E19\u0E15\u0E31\u0E27\u0E02\u0E2D\u0E07 Python \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 `stderr` \u0E44\u0E14\u0E49\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E27\u0E34\u0E18\u0E35\
  \u0E19\u0E35\u0E49\u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\u0E22\u0E17\u0E35\u0E48\u0E07\
  \u0E48\u0E32\u0E22."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีการ:


### การใช้ `sys.stderr`
โมดูล `sys` ที่มีอยู่ในตัวของ Python ช่วยให้สามารถเขียนไปยัง `stderr` ได้อย่างชัดเจน วิธีนี้เหมาะสำหรับการส่งข้อความข้อผิดพลาดหรือการวินิจฉัยที่ง่าย

```python
import sys

sys.stderr.write('ข้อผิดพลาด: เกิดข้อผิดพลาดบางอย่าง.\n')
```
ผลลัพธ์ตัวอย่าง (ไปที่ stderr):
```
ข้อผิดพลาด: เกิดข้อผิดพลาดบางอย่าง.
```

### การใช้ฟังก์ชัน `print`
ฟังก์ชัน `print` ของ Python สามารถเปลี่ยนเส้นทางออกของมันไปยัง `stderr` โดยการระบุพารามิเตอร์ `file` วิธีนี้มีประโยชน์สำหรับการใช้ความสามารถที่ใช้งานง่ายของ `print` ในขณะที่จัดการกับข้อความข้อผิดพลาด
```python
from sys import stderr

print('ข้อผิดพลาด: ล้มเหลวในโมดูล.', file=stderr)
```
ผลลัพธ์ตัวอย่าง (ไปที่ stderr):
```
ข้อผิดพลาด: ล้มเหลวในโมดูล.
```

### การใช้โมดูล `logging`
สำหรับวิธีการที่ครอบคลุมยิ่งขึ้น, โมดูล `logging` ของ Python สามารถส่งข้อความไปยัง `stderr` และอื่น ๆ อีกมากมาย, เช่น การเขียนไปยังไฟล์หรือการปรับแต่งรูปแบบข้อความ วิธีนี้เหมาะสมที่สุดสำหรับแอปพลิเคชันที่ต้องการระดับการบันทึกที่แตกต่างกัน, การจัดรูปแบบข้อความ, หรือปลายทาง
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('ข้อผิดพลาด: การเชื่อมต่อฐานข้อมูลล้มเหลว.')
```
ผลลัพธ์ตัวอย่าง (ไปที่ stderr):
```
ERROR:__main__:ข้อผิดพลาด: การเชื่อมต่อฐานข้อมูลล้มเหลว.
```

### ไลบรารีภายนอก: `loguru`
`loguru` เป็นไลบรารีภายนอกที่ได้รับความนิยมซึ่งทำให้กระบวนการบันทึกในแอปพลิเคชัน Python ง่ายขึ้น มันสามารถส่งข้อผิดพลาดไปยัง `stderr` อัตโนมัติ ในหมู่คุณสมบัติอื่น ๆ

เพื่อใช้ `loguru`, ก่อนอื่นต้องติดตั้งผ่าน pip:
```shell
pip install loguru
```

จากนั้น, นำไปใช้กับสคริปต์ Python ของคุณดังนี้:
```python
from loguru import logger

logger.error('ข้อผิดพลาด: ไม่สามารถเปิดไฟล์ได้.')
```
ผลลัพธ์ตัวอย่าง (ไปที่ stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - ข้อผิดพลาด: ไม่สามารถเปิดไฟล์ได้.
```

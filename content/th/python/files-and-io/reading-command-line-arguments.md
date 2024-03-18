---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:43.715994-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C Python\
  \ \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E1B\u0E49\u0E2D\u0E19\u0E40\u0E02\u0E49\u0E32\u0E08\u0E32\
  \u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\
  \u0E21\u0E34\u0E19\u0E31\u0E25\u0E44\u0E14\u0E49 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E2D\
  \u0E30\u0E44\u0E23?\u2026"
lastmod: '2024-03-17T21:57:55.777791-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C Python\
  \ \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E1B\u0E49\u0E2D\u0E19\u0E40\u0E02\u0E49\u0E32\u0E08\u0E32\
  \u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\
  \u0E21\u0E34\u0E19\u0E31\u0E25\u0E44\u0E14\u0E49 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E2D\
  \u0E30\u0E44\u0E23?\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การอ่านอาร์กิวเมนต์บน command line ช่วยให้สคริปต์ Python ของคุณสามารถใช้งานร่วมกับข้อมูลป้อนเข้าจากผู้ใช้ในเทอร์มินัลได้ เพราะอะไร? เพราะความยืดหยุ่นเป็นสิ่งสำคัญ; ผู้ใช้สามารถปรับเปลี่ยนพฤติกรรมโดยไม่ต้องแก้ไขโค้ดที่คุณต้องการเก็บรักษาไว้

## วิธีการ:

โดยใช้โมดูล `sys` ของ Python, คุณสามารถจับอาร์กิวเมนต์บน command line ได้อย่างง่ายดาย นี่คือวิธีการเข้าถึงมันในสคริปต์ของคุณ:

```python
import sys

# อาร์กิวเมนต์ตัวแรกเป็นชื่อสคริปต์เสมอ, ดังนั้นเราจะข้ามมัน
arguments = sys.argv[1:]

# ทำบางอย่างกับอาร์กิวเมนต์
print("คุณได้ป้อน:", arguments)
```

เรียกใช้สคริปต์ของคุณอย่างนี้:

```bash
python your_script.py these are your arguments
```

ตัวอย่างผลลัพธ์:

```
คุณได้ป้อน: ['these', 'are', 'your', 'arguments']
```

## ลงลึก

เมื่อก่อน, ผู้คนโต้ตอบกับคอมพิวเตอร์ผ่าน command lines นั่นเป็นเหตุผลที่ภาษาส่วนใหญ่รวมถึง Python มีวิธีอ่านอาร์กิวเมนต์บน command line นี่คือวิธีที่สคริปต์ถูกควบคุมก่อนที่ GUIs จะเกิดขึ้น

`sys.argv` ของ Python นั้นมีประโยชน์, แต่สำหรับการแยกคำสั่งยิ่งกว่านั้น, มี `argparse` `argparse` เป็นโมดูลสำหรับเมื่อคุณต้องการมากกว่าพื้นฐาน - เช่นเมื่ออาร์กิวเมนต์ของคุณต้องการชื่อ, ประเภท, หรือค่าเริ่มต้น

ตอนนี้, `sys.argv` เป็นแค่ลิสต์ ทุกอย่างที่คุณผ่านไปเป็นสตริง, ไม่ว่าอะไรก็ตาม ไม่มีวิธีเวทมนตร์; ถ้าคุณต้องการตัวเลข, คุณต้องแปลงมันเองด้วยอะไรสักอย่างเช่น `int()` หรือ `float()`

## ดูเพิ่มเติม

สำหรับข้อมูลเพิ่มเติมเกี่ยวกับ `sys.argv` และ `argparse`, ดูได้จากเอกสารของ Python:

- `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- บทแนะนำ `argparse`: https://docs.python.org/3/howto/argparse.html 

และถ้าคุณต้องการดำดิ่งลึกลงไปอีกในอินเตอร์เฟซบรรทัดคำสั่ง:

- Click: https://click.palletsprojects.com/en/7.x/
- docopt: http://docopt.org/ 

สนุกกับการเขียนโค้ด!

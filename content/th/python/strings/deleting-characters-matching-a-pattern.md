---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-04-04T01:28:06.848478-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E35\u0E48\
  \u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A"
weight: 5
---

## วิธีการ:
```Python
import re

# ตัวอย่างสตริง
text = "Hello, World! 1234"

# ลบตัวเลขทั้งหมด
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # ผลลัพธ์: "Hello, World! "

# ลบเครื่องหมายวรรคตอน
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # ผลลัพธ์: "Hello World 1234"

# ลบสระ
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # ผลลัพธ์: "Hll, Wrld! 1234"
```

### ฟังก์ชันที่ฉันเขียนขึ้น

ฉันทำสิ่งนี้บ่อยพอที่ฉันได้รีแฟคเตอร์มันเป็นฟังก์ชัน `delete()` นี้ นอกจากนี้ยังเป็นตัวอย่างที่ดีของ [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## ดำดิ่งลึกลงไป
การลบอักขระที่ตรงกับรูปแบบในข้อความมีรากฐานลึกในวิทยาการคอมพิวเตอร์ย้อนกลับไปถึงเครื่องมือ Unix แต่เนิ่นๆ เช่น `sed` และ `grep` ใน Python, โมดูล `re` นำเสนอความสามารถนี้ โดยใช้ regular expressions—เครื่องมือที่ทรงพลังและหลากหลายสำหรับการประมวลผลข้อความ

ทางเลือกอื่นสำหรับโมดูล `re` รวมถึง:
- วิธีการของสตริงเช่น `replace()` สำหรับกรณีง่ายๆ
- ไลบรารีบุคคลที่สามเช่น `regex` สำหรับรูปแบบที่ซับซ้อนมากขึ้นและการสนับสนุน Unicode ที่ดีกว่า

ภายใน, เมื่อคุณใช้ `re.sub()`, ตัวแปล Python จะคอมไพล์รูปแบบเป็นชุดของ bytecodes, ซึ่งจะถูกประมวลผลโดยเครื่องจักรสถานะที่ดำเนินการ pattern-matching โดยตรงกับข้อความนำเข้า การดำเนินการนี้อาจใช้ทรัพยากรสูงสำหรับสตริงขนาดใหญ่หรือรูปแบบที่ซับซ้อน ดังนั้น การพิจารณาประสิทธิภาพจึงเป็นสิ่งสำคัญสำหรับการประมวลผลข้อมูลขนาดใหญ่

## ดูเพิ่มเติม
- [เอกสารประกอบโมดูล Python `re`](https://docs.python.org/3/library/re.html): เอกสารอย่างเป็นทางการสำหรับ regular expressions ใน Python
- [Regular-Expressions.info](https://www.regular-expressions.info/): คู่มือละเอียดสำหรับ regular expressions
- [บทเรียนจาก Real Python เกี่ยวกับ regex](https://realpython.com/regex-python/): การประยุกต์ใช้ regular expressions ใน Python ในโลกแห่งความจริง

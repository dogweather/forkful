---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E09\u0E31\u0E19\u0E17\
  \u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E1A\u0E48\u0E2D\u0E22\u0E1E\u0E2D\
  \u0E17\u0E35\u0E48\u0E08\u0E30\u0E23\u0E35\u0E41\u0E1F\u0E01\u0E17\u0E2D\u0E23\u0E4C\
  \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19 `delete()` \u0E07\u0E48\u0E32\u0E22\u0E46 \u0E19\u0E35\u0E49 \u0E19\u0E35\
  \u0E48\u0E22\u0E31\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E2A\u0E32\u0E18\
  \u0E34\u0E15\u0E17\u0E35\u0E48\u0E14\u0E35\u0E02\u0E2D\u0E07\u2026"
lastmod: '2024-04-05T21:54:01.146491-06:00'
model: gpt-4-0125-preview
summary: "\u0E09\u0E31\u0E19\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\
  \u0E1A\u0E48\u0E2D\u0E22\u0E1E\u0E2D\u0E17\u0E35\u0E48\u0E08\u0E30\u0E23\u0E35\u0E41\
  \u0E1F\u0E01\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E1F\
  \u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `delete()` \u0E07\u0E48\u0E32\u0E22\u0E46\
  \ \u0E19\u0E35\u0E49 \u0E19\u0E35\u0E48\u0E22\u0E31\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E01\u0E32\u0E23\u0E2A\u0E32\u0E18\u0E34\u0E15\u0E17\u0E35\u0E48\u0E14\u0E35\u0E02\
  \u0E2D\u0E07 [doctests](https://docs.python.org/3/library/doctest.html)."
title: "\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E35\u0E48\
  \u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A"
weight: 5
---

## วิธีการ:
```Python
import re

# สตริงตัวอย่าง
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

### ฟังก์ชันที่ฉันสร้างขึ้น

ฉันทำสิ่งนี้บ่อยพอที่จะรีแฟกทอร์มันเป็นฟังก์ชัน `delete()` ง่ายๆ นี้ นี่ยังเป็นการสาธิตที่ดีของ [doctests](https://docs.python.org/3/library/doctest.html):

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



## การศึกษาลึกลงไป
การลบอักขระที่ตรงกับรูปแบบในข้อความมีรากฐานลึกซึ้งในวิทยาการคอมพิวเตอร์ ย้อนกลับไปยังเครื่องมือแรกๆ ของ Unix เช่น `sed` และ `grep` ใน Python, โมดูล `re` ให้ความสามารถนี้ โดยใช้นิพจน์ปรกติ—เครื่องมือที่ทรงพลังและหลากหลายสำหรับการประมวลผลข้อความ

ทางเลือกอื่นๆ สำหรับโมดูล `re` ประกอบด้วย:
- วิธีการสตริง เช่น `replace()` สำหรับกรณีง่ายๆ
- ไลบรารีบุคคลที่สาม เช่น `regex` สำหรับรูปแบบที่ซับซ้อนและการสนับสนุน Unicode ที่ดีกว่า

ภายใต้ฝาครอบ, เมื่อคุณใช้ `re.sub()`, ตัวแปลภาษา Python จะคอมไพล์รูปแบบเป็นชุดของไบต์โค้ด ซึ่งได้รับการประมวลผลโดยเครื่องจักรสถานะที่ทำการจับคู่รูปแบบโดยตรงกับข้อความนำเข้า การดำเนินการนี้อาจใช้ทรัพยากรมากสำหรับสตริงขนาดใหญ่หรือรูปแบบที่ซับซ้อน ดังนั้นการพิจารณาประสิทธิภาพเป็นสิ่งสำคัญสำหรับการประมวลผลข้อมูลขนาดใหญ่

## ดูเพิ่มเติม
- [เอกสารของโมดูล Python `re`](https://docs.python.org/3/library/re.html): เอกสารอย่างเป็นทางการสำหรับนิพจน์ปรกติใน Python
- [Regular-Expressions.info](https://www.regular-expressions.info/): คู่มืออย่างละเอียดเกี่ยวกับนิพจน์ปรกติ
- [บทเรียนจาก Real Python เกี่ยวกับ regex](https://realpython.com/regex-python/): การใช้งานนิพจน์ปรกติใน Python ในโลกแห่งความเป็นจริง

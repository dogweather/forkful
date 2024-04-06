---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:22.240219-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E43\
  \u0E0A\u0E49 regex \u0E43\u0E19 Python \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E42\u0E21\u0E14\u0E39\u0E25 `re` \u0E0B\u0E36\
  \u0E48\u0E07\u0E21\u0E35\u0E0A\u0E38\u0E14\u0E02\u0E2D\u0E07\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E30\u0E21\u0E27\
  \u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49 regular expressions."
lastmod: '2024-04-05T21:54:01.156061-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 regex \u0E43\u0E19 Python \u0E40\u0E01\
  \u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `re` \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E0A\u0E38\u0E14\u0E02\u0E2D\
  \u0E07\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 regular expressions."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:
การใช้ regex ใน Python เกี่ยวข้องกับโมดูล `re` ซึ่งมีชุดของฟังก์ชันเพื่อประมวลผลข้อความโดยใช้ regular expressions

### การจับคู่รูปแบบพื้นฐาน
เพื่อค้นหารูปแบบในสตริง ให้ใช้ `re.search()` จะมีการคืนค่าเป็นอ็อบเจกต์ match เมื่อพบรูปแบบนั้น ไม่เช่นนั้นจะเป็น `None`
```python
import re

text = "เรียนภาษา Python"
match = re.search("Python", text)
if match:
    print("พบรูปแบบ!")
else:
    print("ไม่พบรูปแบบ.")
```
ผลลัพธ์:
```
พบรูปแบบ!
```

### การคอมไพล์ Regular Expressions
สำหรับการใช้รูปแบบเดียวกันซ้ำๆ คอมไพล์มันไว้ก่อนด้วย `re.compile()` เพื่อประสิทธิภาพที่ดีขึ้น
```python
pattern = re.compile("Python")
match = pattern.search("เรียนภาษา Python")
if match:
    print("พบรูปแบบที่คอมไพล์!")
```
ผลลัพธ์:
```
พบรูปแบบที่คอมไพล์!
```

### การแบ่งสตริง
เพื่อแบ่งสตริงทุกครั้งที่พบกับรูปแบบ regex ให้ใช้ `re.split()`
```python
result = re.split("\s", "Python สนุก")
print(result)
```
ผลลัพธ์:
```
['Python', 'สนุก']
```

### การหาคำที่ตรงกันทั้งหมด
เพื่อหาทุกครั้งที่ไม่ซ้อนทับกันของรูปแบบ ใช้ `re.findall()`
```python
matches = re.findall("น", "การเขียนโปรแกรม Python")
print(matches)
```
ผลลัพธ์:
```
['น', 'น']
```

### การแทนที่ข้อความ
ใช้ `re.sub()` เพื่อแทนที่รูปแบบที่พบด้วยสตริงใหม่
```python
replaced_text = re.sub("สนุก", "เจ๋ง", "Python สนุก")
print(replaced_text)
```
ผลลัพธ์:
```
Python เจ๋ง
```

### ไลบรารี่ของบุคคลที่สาม
แม้โมดูล `re` ที่มีอยู่ใน Python จะมีประสิทธิภาพสูง ไลบรารีของบุคคลที่สามเช่น `regex` มีคุณสมบัติเพิ่มเติมและประสิทธิภาพที่เหนือกว่า เพื่อใช้ `regex` ติดตั้งผ่าน pip (`pip install regex`) และนำเข้าในโค้ดของคุณ

```python
import regex

text = "เรียน Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"พบเวอร์ชัน: {match.group(1)}")
```
ผลลัพธ์:
```
พบเวอร์ชัน: 3.8
```

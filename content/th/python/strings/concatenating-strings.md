---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:26.800496-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E48\u0E2D\u0E17\
  \u0E49\u0E32\u0E22\u0E01\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E21\u0E48 \u0E21\u0E31\u0E19\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E25\
  \u0E48\u0E19\u0E40\u0E25\u0E42\u0E01\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E40\u0E23\
  \u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E19\u0E36\
  \u0E01\u0E16\u0E36\u0E07\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  ,\u2026"
lastmod: '2024-03-17T21:57:55.753774-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E48\u0E2D\u0E17\
  \u0E49\u0E32\u0E22\u0E01\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E21\u0E48 \u0E21\u0E31\u0E19\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E25\
  \u0E48\u0E19\u0E40\u0E25\u0E42\u0E01\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E40\u0E23\
  \u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E19\u0E36\
  \u0E01\u0E16\u0E36\u0E07\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  ,\u2026"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การต่อสตริงหมายถึงการนำสตริงมาต่อกันต่อท้ายกันเพื่อสร้างสตริงใหม่ มันเหมือนกับการเล่นเลโก้สตริง เราทำแบบนี้เพื่อสร้างข้อความ นึกถึงชื่อผู้ใช้, ข้อความแสดงข้อผิดพลาด, และเนื้อหาที่เปลี่ยนแปลงได้

## วิธีทำ:
ลองมาต่อสตริงกันเลย

```python
first_name = "Charlie"
last_name = "Brown"
full_name = first_name + " " + last_name  # การต่อสตริงแบบคลาสสิคพร้อมช่องว่าง
print(full_name)
```
ผลลัพธ์: `Charlie Brown`

ใช้ `join()` สำหรับรายการคำ:

```python
words = ["Hello", "world!"]
sentence = " ".join(words)
print(sentence)
```
ผลลัพธ์: `Hello world!`

F-String (ตั้งแต่ Python 3.6):

```python
user = "snoopy"
action = "flying"
log_message = f"{user} is {action} his doghouse"
print(log_message)
```
ผลลัพธ์: `snoopy is flying his doghouse`

## ลงลึก
การต่อสตริงเป็นการดำเนินการพื้นฐานของสตริงนับตั้งแต่เริ่มต้นการเขียนโปรแกรม จำไว้ว่า Python รักษาสตริงเป็น immutable, ดังนั้นการต่อกันทุกครั้งจะสร้างสตริงใหม่

ในอดีต, บวก (`+`) เป็นสิ่งเดียวที่เรามี ไม่มีประสิทธิภาพสำหรับสตริงหลายๆ ตัว เนื่องจากอาจนำไปสู่การบวมของหน่วยความจำและประสิทธิภาพการทำงานที่ช้า แนะนำวิธีการ `join()`—เป็นมิตรกับหน่วยความจำมากขึ้นโดยเฉพาะสำหรับการผสานรวมสตริงเป็นลำดับ

F-Strings, ซึ่งเปิดตัวใน Python 3.6, เป็นการเปลี่ยนเกม พวกเขาอ่านง่ายและรวดเร็ว และอนุญาตให้มีการประเมินออกจากสตริง—`f"{variable}"` พวกเขาเป็นที่นิยมสำหรับนักพัฒนา Python ยุคใหม่, ผสมผสานความสามารถและประสิทธิภาพ

## ดูเพิ่มเติม
- [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- [วิธีการจัดรูปแบบสตริงที่ดีที่สุดใน Python](https://realpython.com/python-f-strings/)

---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:03.760077-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21 (String interpolation) \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E1D\u0E31\u0E07\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E41\u0E1A\u0E1A\u0E04\u0E07\u0E17\u0E35\u0E48\u2026"
lastmod: '2024-03-17T21:57:55.748216-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21 (String interpolation) \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E1D\u0E31\u0E07\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E41\u0E1A\u0E1A\u0E04\u0E07\u0E17\u0E35\u0E48\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## อะไร & ทำไม?
การแทรกข้อความ (String interpolation) เป็นวิธีการฝังนิพจน์เข้าไปในตัวอักษรแบบคงที่ โปรแกรมเมอร์ใช้วิธีนี้เพื่อแทรกค่าเข้าไปในสตริงด้วยวิธีที่ให้โค้ดอ่านง่ายและสะอาดกว่าการต่อสตริงแบบดั้งเดิม

## วิธีการ:
ใน Python 3.6 ขึ้นไป คุณสามารถแทรกสตริงโดยใช้ f-strings ดังนี้:

```Python
name = 'Alice'
age = 30
greeting = f"สวัสดี, {name}. คุณอายุ {age} ปีแล้ว."

print(greeting)
```

ผลลัพธ์:
```
สวัสดี, Alice. คุณอายุ 30 ปีแล้ว.
```

คุณยังสามารถใช้นิพจน์ภายในวงเล็บปีกกาได้:

```Python
a = 5
b = 10
info = f"ห้าบวกสิบเท่ากับ {a + b}, ไม่ใช่ {2 * (a + b)}."

print(info)
```

ผลลัพธ์:
```
ห้าบวกสิบเท่ากับ 15, ไม่ใช่ 30.
```

## ดูให้ลึกลง
ก่อน Python 3.6, `.format()` เป็นวิธีการที่คนนิยมใช้สำหรับการแทรกสตริง:

```Python
name = 'Bob'
age = 25
greeting = "สวัสดี, {}. คุณอายุ {} ปีแล้ว.".format(name, age)

print(greeting)
```

Python เวอร์ชันเก่า (เวอร์ชัน < 2.6) ใช้ตัวดำเนินการ `%` สำหรับการแทรกสตริง ซึ่งน้อยความเข้าใจและอาจสร้างความยุ่งยากได้เมื่อมีตัวแปรหลายตัว:

```Python
name = 'Carol'
age = 35
greeting = "สวัสดี, %s. คุณอายุ %d ปีแล้ว." % (name, age)

print(greeting)
```

นอกจากไวยากรณ์ที่สะอาดกว่า f-strings ยังเร็วกว่าเพราะว่าถูกประเมินในเวลาที่โปรแกรมทำงานและจากนั้นจะถูกแปลงเป็นการดำเนินการรูปแบบสตริงที่มีประสิทธิภาพโดยตรง วิธี `.format()` และตัวดำเนินการ `%` มีขั้นตอนมากกว่าและช้ากว่า

## ดูเพิ่มเติม
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) สำหรับเอกสารการใช้งาน f-strings อย่างเป็นทางการ
- [Python f-strings](https://realpython.com/python-f-strings/) โดย Real Python เพื่อบทช่วยสอนเกี่ยวกับการใช้งาน f-strings
- [วิธีการ .format()](https://docs.python.org/3/library/stdtypes.html#str.format) ในเอกสารการใช้งาน Python เพื่อเข้าใจวิธีการจัดรูปแบบสตริงแบบเก่า `.format()`

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:30.727515-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E2A\
  \u0E23\u0E49\u0E32\u0E07 dictionary \u0E43\u0E19 Python \u0E17\u0E33\u0E44\u0E14\
  \u0E49\u0E07\u0E48\u0E32\u0E22 \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E43\u0E2A\u0E48\
  \u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07 key-value \u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19\
  \ curly braces `{}`, \u0E42\u0E14\u0E22\u0E21\u0E35 key \u0E41\u0E25\u0E30 value\
  \ \u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E01\u0E31\u0E19\u0E14\u0E49\u0E27\u0E22\
  \ colon."
lastmod: '2024-03-17T21:57:55.754678-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07 dictionary \u0E43\u0E19\
  \ Python \u0E17\u0E33\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22 \u0E43\u0E2B\u0E49\
  \u0E04\u0E38\u0E13\u0E43\u0E2A\u0E48\u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07 key-value\
  \ \u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19 curly braces `{}`, \u0E42\u0E14\u0E22\u0E21\
  \u0E35 key \u0E41\u0E25\u0E30 value \u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E01\u0E31\
  \u0E19\u0E14\u0E49\u0E27\u0E22 colon."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## วิธีการ:
การสร้าง dictionary ใน Python ทำได้ง่าย ให้คุณใส่คู่ของ key-value ลงไปใน curly braces `{}`, โดยมี key และ value แยกจากกันด้วย colon:

```Python
# สร้าง associative array (dictionary)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

ผลลัพธ์:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

การเข้าถึงค่าด้วย key ทำได้ง่ายๆ:

```Python
# เข้าถึงค่า
print(my_dict["name"])
```

ผลลัพธ์:
```
John
```

การเพิ่มหรือปรับปรุง elements ทำได้โดยการกำหนดค่าให้กับ key:

```Python
# เพิ่มคู่ของ key-value ใหม่
my_dict["email"] = "john@example.com"
# ปรับปรุงค่า
my_dict["age"] = 31
print(my_dict)
```

ผลลัพธ์:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

เพื่อวนซ้ำผ่านทาง item ของ dictionary:

```Python
# วนซ้ำผ่านคู่ของ key-value
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

ผลลัพธ์:
```
name: John
age: 31
city: New York
email: john@example.com
```

## ลงลึก
Associative arrays ใน Python, หรือ dictionaries, ถูกนำมาใช้เพื่อเป็นโครงสร้างข้อมูลสำหรับการเข้าถึงและการจัดการข้อมูลอย่างมีประสิทธิภาพ ไม่เหมือนกับลำดับข้อมูล, ซึ่งถูกเข้าถึงด้วยช่วงของตัวเลข, dictionaries ถูกเข้าถึงด้วยค่า key, ซึ่งสามารถเป็นประเภทที่ไม่สามารถเปลี่ยนแปลงได้ เลือกใช้โครงสร้างนี้ทำให้ dictionaries เหมาะสำหรับตาราง lookup ที่รวดเร็วซึ่ง key ส่งไปยังค่าที่ไม่ซ้ำกัน

ทางประวัติศาสตร์แล้ว, dictionaries ใน Python ถูกจัดทำโดยใช้ hash table, ซึ่งรับประกันว่าความซับซ้อนของเวลาเฉลี่ยสำหรับการค้นหา, การแทรก, และการลบการดำเนินการคือ O(1) ใน Python 3.6 และในภายหลัง, dictionaries ยังรักษาการเรียงลำดับของรายการที่ถูกเพิ่มเข้ามา, รวมประโยชน์ของ hash tables กับความสามารถคาดการณ์ของลำดับการใส่ข้อมูลที่เห็นในโครงสร้างข้อมูลที่เรียงลำดับ

แม้ว่า dictionaries มีความหลากหลายอย่างมาก, ในบางกรณีพิเศษ, ตัวเลือกอื่นๆ เช่น `collections.defaultdict` หรือ `collections.OrderedDict` (ก่อน Python 3.7) อาจจะเป็นตัวเลือกที่ดีกว่า `defaultdict` มีประโยชน์โดยเฉพาะเมื่อคุณต้องการ dictionary ที่ส่งคืนค่าเริ่มต้นสำหรับคีย์ที่ไม่มีอยู่จริง, ทำให้ง่ายขึ้นในการจัดการกับบางประเภทของตรรกะเงื่อนไข อย่างไรก็ตาม, ด้วยการปรับปรุงและวิวัฒนาการอย่างต่อเนื่องของ Python, class dictionary ที่มาพร้อมกับฟังก์ชันเดิมมักจะยังคงเป็นตัวเลือกแรกสำหรับ associative arrays เนื่องจากความแข็งแกร่งและความสะดวกสบายที่มันเสนอไว้เลยตั้งแต่แรก

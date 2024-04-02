---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:42.905359-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\
  \u0E15\u0E31\u0E14\u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E32\u0E01\
  \u0E2A\u0E32\u0E22\u0E23\u0E34\u0E1A\u0E1A\u0E34\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25, \u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:55.751045-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\
  \u0E15\u0E31\u0E14\u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E32\u0E01\
  \u0E2A\u0E32\u0E22\u0E23\u0E34\u0E1A\u0E1A\u0E34\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25, \u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไรและทำไม?
การดึงข้อความย่อยหมายถึงการดึงส่วนที่เฉพาะเจาะจงของสตริงออกมา เช่นการตัดชิ้นส่วนจากสายริบบิ้น โปรแกรมเมอร์ทำเช่นนี้เพื่อแยกข้อมูล, แยกข้อมูล หรือเพียงแค่จัดการกับข้อความ

## วิธีการ:
```Python
# การใช้เครื่องหมายการตัด
text = "Python rocks!"
substring = text[7:12]
print(substring)  # ผลลัพธ์: rocks

# การใช้ฟังก์ชัน slice()
slice_object = slice(7, 12)
print(text[slice_object])  # ผลลัพธ์: rocks

# การใช้ str.split() และการเข้าถึงองค์ประกอบ
parts = text.split()
print(parts[1])  # ผลลัพธ์: rocks!
```

## ศึกษาลึกลงไป
ในอดีต, แนวคิดของการจัดการสตริง, รวมถึงการดึงข้อความย่อย, เป็นสิ่งสำคัญในภาษาการเขียนโปรแกรมแรก ๆ เช่น C ซึ่งเป็นงานที่ซับซ้อนกว่าที่เกี่ยวข้องกับการใช้พอยน์เตอร์ ด้วย Python, ความง่ายของมันถูกระดับขึ้นไปถึงสิบเอ็ด - มีสัญชาตญาณมากขึ้นและมีโอกาสทำผิดพลาดน้อยลง

Python มีวิธีการหลายอย่างสำหรับการดึงข้อความย่อย ในขณะที่ตัวอย่างที่ใช้วิธีการตัดนั้นตรงไปตรงมามาก แต่วิธีการเช่น `split()` อาจจะเป็นประโยชน์เมื่อคุณกำลังจัดการกับตัวแบ่งหรือช่องว่าง

ลึกลงไป, สตริง Python เป็นอาร์เรย์ของไบต์ที่แทนด้วยอักขระ Unicode แต่แตกต่างจากอาร์เรย์ในภาษาอื่น, สตริง Python ไม่สามารถเปลี่ยนแปลงได้หลังจากสร้างแล้ว แง่มุมนี้สำคัญเมื่อเข้าใจว่าทำไมการดำเนินการข้อความย่อยไม่ได้แก้ไขสตริงต้นฉบับ แต่สร้างข้อความใหม่แทน

## ดูเพิ่มเติม
- เอกสาร Python เกี่ยวกับวิธีการสตริง: https://docs.python.org/3/library/stdtypes.html#string-methods
- บทความเกี่ยวกับการดำเนินการสตริง Python มากขึ้น: https://realpython.com/python-strings/
- บทช่วยสอนการตัดสตริง Python จาก W3Schools: https://www.w3schools.com/python/python_strings_slicing.asp

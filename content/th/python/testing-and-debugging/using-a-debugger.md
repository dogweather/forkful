---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:08.753842-06:00
description: "\u0E21\u0E32\u0E41\u0E22\u0E01\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `pdb`,\
  \ Debugger \u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\
  \u0E1A Python \u0E01\u0E31\u0E19 \u0E08\u0E34\u0E19\u0E15\u0E19\u0E32\u0E01\u0E32\
  \u0E23\u0E16\u0E36\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E37\u0E48\u0E2D `buggy.py`\
  \ \u0E17\u0E35\u0E48\u0E21\u0E35\u0E1A\u0E31\u0E4A\u0E01\u0E41\u0E2D\u0E1A\u0E41\
  \u0E1D\u0E07\u0E2D\u0E22\u0E39\u0E48: ```Python def add_one(number): result = number\u2026"
lastmod: '2024-03-17T21:57:55.766820-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E41\u0E22\u0E01\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `pdb`,\
  \ Debugger \u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\
  \u0E1A Python \u0E01\u0E31\u0E19 \u0E08\u0E34\u0E19\u0E15\u0E19\u0E32\u0E01\u0E32\
  \u0E23\u0E16\u0E36\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E37\u0E48\u0E2D `buggy.py`\
  \ \u0E17\u0E35\u0E48\u0E21\u0E35\u0E1A\u0E31\u0E4A\u0E01\u0E41\u0E2D\u0E1A\u0E41\
  \u0E1D\u0E07\u0E2D\u0E22\u0E39\u0E48: ```Python def add_one(number): result = number\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
มาแยกการใช้ `pdb`, Debugger ที่มาพร้อมกับ Python กัน จินตนาการถึงไฟล์ชื่อ `buggy.py` ที่มีบั๊กแอบแฝงอยู่:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

เมื่อเรียกใช้สคริปต์นี้ คุณคาดหวังว่าจะได้ `8`, แต่กลับได้แค่ข้อผิดพลาดทางไวยากรณ์ ถึงเวลาใช้ Debugger แล้ว!

ในเทอร์มินัล ให้รัน:
```bash
python -m pdb buggy.py
```

คุณจะเข้าสู่ Debugger และมันจะดูเป็นแบบนี้:
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

ใช้ `l(ist)` เพื่อดูโค้ดเพิ่มเติม, `n(ext)` เพื่อไปยังบรรทัดถัดไป, หรือ `c(ontinue)` เพื่อดำเนินการรันสคริปต์ต่อ หากคุณพบกับข้อผิดพลาด, `pdb` จะหยุดและปล่อยให้คุณตรวจสอบ

หลังจากที่คุณแก้ไข `number ++ 1` เป็น `number + 1`, รีสตาร์ท Debugger เพื่อทดสอบการแก้ไข
จำไว้ว่า, คนเป็นเพื่อนกันไม่ให้เพื่อนเขียนโค้ดโดยไม่มีเครื่องมือช่วย. พอกันที.

## ขุดลึกลงไป
ในยุคมืดของการเขียนโปรแกรม (กล่าวคือ ก่อนที่สภาพแวดล้อมการพัฒนาแบบบูรณาการ, หรือ IDEs, จะเป็นที่นิยมไปทั่ว), Debugger มักจะเป็นเครื่องมือแยกที่คุณใช้นอกตัวแก้ไขข้อความ พวกมันมาช่วยเหลือด้วยการอนุญาตให้โปรแกรมเมอร์ตรวจสอบสถานะซอฟต์แวร์ของพวกเขาในจุดต่างๆ ของการดำเนินการ

ณ ปี 2023, Python's `pdb` ไม่ใช่ตัวเลือกเดียวในพื้นที่นี้ IDEs เช่น PyCharm หรือ Visual Studio Code, ซึ่งมี Debugger ของตัวเองที่มีความพิเศษ มีฟีเจอร์เจ๋งๆ เช่น การตั้งจุดหยุด (breakpoints) ที่คุณสามารถตั้งได้ด้วยการคลิกเมาส์ ไม่ต้องพิมพ์คำสั่งที่ซับซ้อน

จากนั้นมี `ipdb`, แพ็คเกจที่ติดตั้งได้ผ่าน pip ซึ่งนำความดีงามของ `IPython` มาสู่การ Debug มันเหมือนกับ `pdb` ที่ได้รับการเสริมสมรรถนะ, มีความสามารถในการเติมข้อความอัตโนมัติและเน้นไวยากรณ์

Debugger ยังแตกต่างกันในแง่ของการดำเนินงาน บางตัวใกล้ชิดกับการดำเนินงานของโปรแกรมในระดับเครื่องจักรหรือรหัสไบต์ ในขณะที่ตัวอื่นๆ อย่าง Debugger ภาษาระดับสูง, รันโค้ดในสภาพแวดล้อมพิเศษที่ตรวจสอบสถานะตัวแปรและควบคุมการไหลของการดำเนินการ

## ดูเพิ่มเติม
หากต้องการรายละเอียดเพิ่มเติมเกี่ยวกับ Debugger ของ Python เอง, ตรวจสอบที่:
- เอกสาร `pdb`: https://docs.python.org/3/library/pdb.html

หากคุณสนใจตัวเลือกอื่น, ลิงก์เหล่านี้จะช่วยคุณได้:
- ที่เก็บข้อมูลและคู่มือการใช้งาน `ipdb`: https://github.com/gotcha/ipdb
- การ Debug ด้วย Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- ฟีเจอร์การ Debug ของ PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

ขอให้สนุกกับการล่าบั๊ก!

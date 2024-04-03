---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:18.909053-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E43\u0E0A\
  \u0E49\u0E1E\u0E25\u0E31\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\
  \u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E43\u0E19 bash:\
  \ 1. \u0E2A\u0E25\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E20\u0E32\
  \u0E22\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \ `sed`."
lastmod: '2024-03-17T21:57:56.381140-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E1E\u0E25\u0E31\u0E07\u0E02\u0E2D\u0E07\
  \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\
  \u0E17\u0E35\u0E48\u0E43\u0E19 bash."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
นี่คือวิธีที่คุณใช้พลังของการค้นหาและแทนที่ใน bash:

1. สลับข้อความภายในสตริงโดยใช้ `sed`:
```Bash
echo "Hello world" | sed 's/world/universe/'
# ผลลัพธ์: Hello universe
```

2. แทนที่ข้อความในไฟล์ และบันทึกการเปลี่ยนแปลง:
```Bash
sed -i 's/old_text/new_text/g' file.txt
```

3. ใช้ตัวแปรในการค้นหาและแทนที่ข้อความ:
```Bash
old="apple"
new="banana"
sed "s/$old/$new/g" <<< "I like apple pies"
# ผลลัพธ์: I like banana pies
```

จำไว้ว่า `g` ที่ต่อท้ายหมายถึง "ทั่วไป" ดังนั้นคุณจะเปลี่ยนทุกการจับคู่ในบรรทัด ไม่ใช่แค่ครั้งแรก

## ลงลึก
เรามีเครื่องมือสำหรับการประมวลผลข้อความในระบบที่คล้ายกับ Unix มานานแล้ว `sed` ย่อมาจาก Stream Editor คือหนึ่งในเครื่องมือเหล่านั้น และมันมีมาตั้งแต่ทศวรรษ 1970s มันไม่ได้เพียงสำหรับการแทนที่อย่างง่าย `sed` ยังสามารถตัดและหั่นข้อความในรูปแบบที่ซับซ้อนได้อีกด้วย

มีทางเลือกอื่น? แน่นอน `awk` มีความก้าวหน้ากว่าเล็กน้อยและสามารถทำงานได้อย่างยอดเยี่ยมกับคอลัมน์และแถว สำหรับการแก้ไขอย่างรวดเร็ว `grep` สามารถช่วยคุณค้นหาสิ่งต่าง ๆ ได้ แต่มันไม่สามารถแทนที่ได้ - เหมือนเป็นคนดู

ภายใต้ฝาครอบ `sed` ใช้ regular expressions ซึ่งเหมือนกับตัวแทนของการค้นหาที่ได้รับสเตียรอยด์ พวกเขาสามารถจับคู่กับรูปแบบใด ๆ ที่คุณสามารถนึกถึงได้ มันทำให้ `sed` มีพลังอย่างมาก แต่ก็เป็นเรื่องยากที่จะเรียนรู้ให้เชี่ยวชาญ

## ดูเพิ่มเติม
- `man sed` สำหรับคู่มือเกี่ยวกับ `sed`
- [บทนำสู่ `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions for Beginners](https://www.regular-expressions.info/tutorial.html)
- The Art of Command Line สำหรับเทคนิค bash เพิ่มเติม (https://github.com/jlevy/the-art-of-command-line)

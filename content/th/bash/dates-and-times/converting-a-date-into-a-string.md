---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:22.350165-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E14\u0E49\u0E32\u0E19\u0E25\
  \u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E02\u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E43\u0E19 Bash."
lastmod: '2024-03-17T21:57:56.408419-06:00'
model: gpt-4-0125-preview
summary: "\u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Bash."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีทำ:
ด้านล่างเป็นตัวอย่างของวิธีการแปลงวันที่เป็นสตริงใน Bash:

```Bash
# แสดงวันที่และเวลาปัจจุบันในรูปแบบเริ่มต้น
echo $(date)

# รูปแบบที่กำหนดเอง: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# รวมเวลา
echo $(date '+%Y-%m-%d %H:%M:%S')

# แปลงวันที่ที่มีอยู่
existing_date='2023-03-17 08:00:00'
date -d "$existing_date" '+%A, %B %d, %Y'
```
ตัวอย่างผลลัพธ์สำหรับคำสั่งข้างต้น:

```
Sat Mar 25 12:04:22 PDT 2023
2023-03-25
2023-03-25 12:04:22
วันศุกร์, มีนาคม 17, 2023
```

## ไดฟ์ลึก
ระบบที่คล้ายกับ Unix ใช้คำสั่ง `date` ตั้งแต่เนิ่นๆ สำหรับการจัดการวันที่และเวลา ความยืดหยุ่นของมันช่วยให้สามารถจัดรูปแบบได้หลากหลาย โดยอาศัยตัวกำหนดรูปแบบเช่น `%Y` สำหรับปี และ `%d` สำหรับวัน

มีทางเลือกอื่นๆ กับคำสั่ง `date` หากคุณใช้เทคโนโลยีสแตคอื่น เช่น Python มี `datetime.strftime`, ในขณะที่ JavaScript เสนอวัตถุ `Date` พร้อมกับเมธอดเช่น `toLocaleDateString()`

เมื่อแปลงวันที่ใน Bash, จำไว้ว่าคำสั่ง `date` สามารถทำงานกับเวลาปัจจุบันของระบบหรือวันที่ที่ให้มา การจัดการเขตเวลาก็สำคัญเช่นกันสำหรับการแปลงวันที่ที่แม่นยำ

## ดูเพิ่มเติม
- GNU coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- คู่มือสคริปต์ขั้นสูงของ Bash: https://tldp.org/LDP/abs/html/
- ตัวกำหนดรูปแบบสำหรับคำสั่งวันที่: https://man7.org/linux/man-pages/man1/date.1.html

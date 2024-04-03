---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:09.845916-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Bash, \u0E04\
  \u0E38\u0E13\u0E43\u0E0A\u0E49 `>&2` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\
  \u0E10\u0E32\u0E19."
lastmod: '2024-03-17T21:57:56.413930-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Bash, \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49 `>&2` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E34\u0E28\u0E17\
  \u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \ stderr \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีการ:
ใน Bash, คุณใช้ `>&2` เพื่อเปลี่ยนทิศทางผลลัพธ์ไปยัง stderr นี่คือตัวอย่างพื้นฐาน:

```bash
echo "This is a normal message"
echo "This is an error message" >&2
```

การรันสคริปต์นี้จะแสดงข้อความทั้งสองที่คอนโซล แต่ถ้าคุณเปลี่ยนทิศทางพวกเขา, คุณสามารถแยก stdout ออกจาก stderr ได้ ตัวอย่างเช่น:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` จะมีข้อความ `"This is a normal message"`, ในขณะที่ `error.txt` จะจับข้อความ `"This is an error message"`.

สำหรับกรณีการใช้งานจริง, พิจารณาสคริปต์ที่ประมวลผลไฟล์และรายงานข้อผิดพลาดหากไฟล์ไม่มีอยู่:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename does not exist!" >&2
    exit 1
else
    echo "Processing $filename"
fi
```

ตัวอย่างผลลัพธ์โดยตรงในคอนโซลเมื่อ `example.txt` ไม่มีอยู่:

```
example.txt does not exist!
```

ไม่มี third-party libraries โดยตรงใน Bash สำหรับการจัดการ stderr เนื่องจากการเปลี่ยนทิศทางได้รับการสนับสนุนโดยเนื้อแท้และโดยทั่วไปแล้วเพียงพอ อย่างไรก็ตาม, สำหรับแอปพลิเคชันที่ซับซ้อน, กรอบการทำงานบันทึกข้อมูลหรือเครื่องมือบันทึกภายนอกเช่น `syslog` หรือ `log4bash` สามารถรวมเข้ากับเพื่อจัดการทั้ง stdout และ stderr ได้อย่างมีประสิทธิภาพยิ่งขึ้น.

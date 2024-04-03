---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:29.678479-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E49\u0E32\u0E16\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E30\
  \u0E21\u0E27\u0E25\u0E1C\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\
  \u0E01\u0E21\u0E32, \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\u0E41\u0E2D\
  \u0E1E, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E1A\u0E31\u0E19\
  \u0E17\u0E36\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.661906-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E49\u0E32\u0E16\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E30\
  \u0E21\u0E27\u0E25\u0E1C\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\
  \u0E01\u0E21\u0E32, \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\u0E41\u0E2D\
  \u0E1E, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E1A\u0E31\u0E19\
  \u0E17\u0E36\u0E01, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\
  \u0E04\u0E48\u0E43\u0E2B\u0E49\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E01\u0E31\u0E1A\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไรและทำไม?
การอ่านไฟล์ข้อความคือการเข้าถึงข้อมูลภายในไฟล์เพื่อประมวลผล โปรแกรมเมอร์ทำเช่นนี้เพื่อดึงข้อมูลออกมา, กำหนดค่าแอพ, วิเคราะห์บันทึก, หรือเพียงแค่ให้ข้อมูลกับสคริปต์

## วิธีการ:
นี่คือข้อมูลอ้างอิงของ Fish Shell ในการเปิดไฟล์ข้อความ:

```Fish Shell
# อ่านไฟล์ทีละบรรทัด
while read -la line
    echo $line
end < file.txt
```

```Fish Shell
# แสดงเนื้อหาของไฟล์โดยตรง
cat file.txt
```

ตัวอย่างผลลัพธ์ (จาก `cat`):

```plaintext
Hello, Fish!
Just swimming through files.
```

## ลงลึก
มีครั้งหนึ่งเล่าว่า แม้กระทั่งก่อนที่ Fish Shell จะเปิดตัวประมาณปี 2005, การอ่านไฟล์เป็นเรื่องจำเป็น Unix shells ได้มีเครื่องมือสำหรับสิ่งนี้เสมอ ทำไม Fish? มันเป็นมิตร, เป็นสมัยใหม่, และมีค่าเริ่มต้นในการเขียนสคริปต์ที่เข้าใจง่าย, ทำให้มันเป็นทางเลือกที่น่าพึงพอใจกว่า shells เก่าๆ

ลูป `while read` เป็นเครื่องมือที่มีประโยชน์สำหรับการปรับเปลี่ยนทีละบรรทัด อย่าลืมว่า `read` มีธงเช่น `-la` สำหรับการสร้างตัวแปรรายการจากบรรทัด—เหมาะสำหรับค่าที่แยกด้วยคอมม่า

ในทางกลับกัน, `cat` เป็นเรื่องง่าย มันรวมและแสดงเนื้อหาไฟล์ มันได้อยู่ใน Unix มาตั้งแต่สมัยนั้น (เอ่อ, 1971 เพื่อเป็นการแน่นอน)

ในแง่ของประสิทธิภาพ, การอ่านโดยตรงโดยทั่วไปเร็วกว่าและโอเคสำหรับไฟล์ขนาดเล็ก แต่เมื่อคุณมีไฟล์ข้อความขนาดใหญ่เช่น Moby Dick, พิจารณาการประมวลผลทีละบรรทัดหรือเครื่องมือเช่น `sed`, `awk`, หรือแม้กระทั่ง `grep` หากคุณกำลังตามหาบรรทัดที่เฉพาะเจาะจง

## ดูเพิ่มเติม
- [เอกสาร Fish อย่างเป็นทางการ](https://fishshell.com/docs/current/index.html) สำหรับการศึกษาลึกเกี่ยวกับทุกสิ่ง Fish Shell
- [กระทู้ Unix StackExchange](https://unix.stackexchange.com/questions/tagged/fish) สำหรับการสนับสนุนและข้อมูลเชิงลึกจากชุมชนที่กว้างขึ้น
- บทแนะนำเกี่ยวกับ [การใช้ awk ในการเขียนสคริปต์ shell](https://www.gnu.org/software/gawk/manual/gawk.html) อาจมีประโยชน์หากมีงานประมวลผลข้อความที่ซับซ้อนขึ้น

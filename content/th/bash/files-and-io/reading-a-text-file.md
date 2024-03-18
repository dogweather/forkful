---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:31.944379-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E08\u0E32\
  \u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\
  \u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\
  \u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.414895-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E08\u0E32\
  \u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\
  \u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\
  \u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การอ่านไฟล์ข้อความหมายถึงการดึงเนื้อหาจากไฟล์เข้าไปในสคริปต์ของคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อทำงานกับข้อมูล, การตั้งค่า, หรือเพื่ออัตโนมัติระบบตามเนื้อหาของไฟล์ข้อความนั้น

## วิธีการ:
นี่คือวิธีที่ง่ายที่สุดในการอ่านไฟล์ทีละบรรทัด:

```Bash
while IFS= read -r line; do
    echo "ข้อความ: $line"
done < "yourfile.txt"
```

ต้องการเนื้อหาทั้งไฟล์พร้อมกันหรือ? ลองใช้วิธีนี้:

```Bash
file_content=$(<yourfile.txt)
echo "$file_content"
```

หรือคุณต้องใช้บรรทัดที่เฉพาะเจาะจง, เช่น บรรทัดที่ 4?

```Bash
sed '4q;d' yourfile.txt
```

ตัวอย่างผลลัพธ์ของการอ่านบรรทัดที่ 4:

```
นี่คือเนื้อหาของบรรทัดที่สี่
```

## ลงลึก
ย้อนกลับไปในวันนั้น, เราไม่มี IDE ที่ซับซ้อน, เรามีเทอร์มินัลและเอดิเตอร์ข้อความง่ายๆ UNIX ถูกออกแบบมาด้วยปรัชญาของการทำหนึ่งอย่างให้ดีที่สุด `cat`, `less`, `sed`, และ `awk` เป็นเหล่าทหารผ่านศึกในการจัดการข้อความ 

การอ่านไฟล์ใน Bash ใช้ประโยชน์จากเครื่องมือเหล่านี้, รวมถึงการเปลี่ยนทิศทางและลูปของ Bash เอง ตัวอย่างเช่น, การใช้ `while` กับ `read` เหมาะสำหรับประสิทธิภาพหน่วยความจำกับไฟล์ขนาดใหญ่ คุณกำลังอ่านทีละบรรทัด, ไม่ได้ดั๊มพ์ทุกอย่างเข้าหน่วยความจำ

`sed` เป็นตัวแก้ไขสตรีม การเลือกบรรทัดที่เฉพาะเจาะจงด้วย `sed '4q;d' yourfile.txt` บอกให้ `sed` หยุดหลังจากบรรทัดที่ 4 (`4q`) จากนั้นจึงพิมพ์ (`;d`) บรรทัดนั้น

มีทางเลือกอื่นๆ อยู่ `awk` เหมาะสมมากสำหรับการประมวลผลข้อความ สคริปต์ Perl และ Python สามารถเรียกใช้ภายใน Bash เมื่อการประมวลผลข้อความซับซ้อนขึ้น แต่ละเครื่องมือและภาษาเหล่านี้มีกรณีการใช้งานและการพิจารณาประสิทธิภาพเฉพาะตัว

## ดูเพิ่มเติม
1. คู่มือ Bash Scripting: https://www.gnu.org/software/bash/manual/
2. `sed` และ `awk` 101 Hacks: https://www.thegeekstuff.com/2009/12/unix-sed-tutorial-6-examples-to-edit-file-in-place/
3. การประมวลผลข้อความบน Command Line ของ Linux ด้วย `grep`, `awk`, `sed`, `sort`, และเพื่อนๆ: https://github.com/learnbyexample/Command-line-text-processing

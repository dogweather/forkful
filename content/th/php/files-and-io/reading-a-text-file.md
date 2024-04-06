---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:26.505658-06:00
description: ''
lastmod: '2024-04-05T22:51:14.378026-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีการ:


### การใช้ `file_get_contents`:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
ตัวอย่างผลลัพธ์:
```
Hello, World!
This is content from the text file.
```

### การใช้ `fopen` และ `fgets`:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
ตัวอย่างผลลัพธ์:
```
Hello, World!
This is content from the text file.
```

### การเขียนไปยังไฟล์ด้วย `file_put_contents`:
```PHP
$newContent = "Adding new text.";
file_put_contents("example.txt", $newContent);
```

## ดำดิ่งลึก
การอ่านไฟล์ข้อความเป็นเรื่องที่เก่าแก่เท่ากับการเขียนโปรแกรมเอง ก่อนที่จะมีฐานข้อมูล ไฟล์กำหนดค่า และข้อมูลผู้ใช้มักจะอยู่ในไฟล์ข้อความง่ายๆ ทางเลือกอื่นๆ เช่น ไฟล์ XML และ JSON มีโครงสร้าง ง่ายต่อการแยกวิเคราะห์ และเหมาะสำหรับข้อมูลที่ซับซ้อน

ใน PHP, `file_get_contents` และ `file()` ใช้สำหรับการอ่านอย่างรวดเร็ว; คำสั่งแรกดึงเนื้อหาทั้งหมดในหนึ่งสตริง และคำสั่งหลังดึงเข้ามาในอะเรย์ `fopen` รวมถึง `fgets` หรือ `fread` ให้คุณควบคุมได้มากกว่า โดยเฉพาะสำหรับไฟล์ขนาดใหญ่ เมื่อคุณอ่านมันทีละบรรทัดหรือเป็นก้อน

ความละเอียดบางประการ: `fopen` ต้องการสิทธิ์ที่เหมาะสม มิฉะนั้นจะล้มเหลว; การจัดการข้อผิดพลาดเป็นปฏิบัติการที่ดีที่สุด เมื่อใช้ `file_put_contents`, ต้องระวังว่าคำสั่งนี้จะเขียนทับไฟล์โดยค่าเริ่มต้น; ใช้ธง `FILE_APPEND` เพื่อเพิ่มเนื้อหาแทน

## ดูเพิ่มเติม
- PHP Manual สำหรับ `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual สำหรับ `fopen`: https://www.php.net/manual/en/function.fopen.php
- PHP Manual สำหรับ `fgets`: https://www.php.net/manual/en/function.fgets.php
- PHP Manual สำหรับ `file_put_contents`: https://www.php.net/manual/en/function.file-put-contents.php
- บทช่วยสอนเกี่ยวกับการจัดการไฟล์ใน PHP: https://www.w3schools.com/php/php_file.asp

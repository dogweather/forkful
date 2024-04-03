---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:27.056977-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E0A\u0E48\u0E27\u0E22\
  \u0E04\u0E38\u0E13\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\
  \u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19 `tmpfile()`, \u0E0B\u0E36\u0E48\u0E07\u0E08\u0E30\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\
  \u0E43\u0E19\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35 temp \u0E02\u0E2D\
  \u0E07\u0E23\u0E30\u0E1A\u0E1A\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E07\u0E48\u0E32\u0E22."
lastmod: '2024-03-17T21:57:56.331303-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E0A\u0E48\u0E27\u0E22\u0E04\u0E38\u0E13\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E14\u0E49\
  \u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `tmpfile()`, \u0E0B\
  \u0E36\u0E48\u0E07\u0E08\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E43\u0E19\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\
  \u0E2D\u0E23\u0E35 temp \u0E02\u0E2D\u0E07\u0E23\u0E30\u0E1A\u0E1A\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22."
title: "\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\u0E48\u0E27\
  \u0E04\u0E23\u0E32\u0E27"
weight: 21
---

## วิธีการ:
PHP ช่วยคุณสร้างไฟล์ชั่วคราวด้วยฟังก์ชัน `tmpfile()`, ซึ่งจะสร้างไฟล์ให้คุณในไดเรกทอรี temp ของระบบของคุณ นี่คือตัวอย่างอย่างง่าย:

```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Hello, temporary world!");
rewind($tempFile);

echo fread($tempFile, 1024); // อ่านสิ่งที่เราเขียนลงในไฟล์

fclose($tempFile); // ไฟล์ชั่วคราวจะถูกลบอัตโนมัติ
?>
```

ตัวอย่างผลลัพธ์:
```
Hello, temporary world!
```

คุณยังสามารถใช้ `tempnam()` เพื่อรับชื่อของไฟล์ที่คุณจัดการเอง:

```PHP
<?php
$tempFilePath = tempnam(sys_get_temp_dir(), 'Tux');
file_put_contents($tempFilePath, "Penguins are cool!");

echo file_get_contents($tempFilePath); // อ่านเนื้อหา

unlink($tempFilePath); // ลบไฟล์เมื่อคุณเสร็จแล้ว
?>
```

ตัวอย่างผลลัพธ์:
```
Penguins are cool!
```

## การศึกษาเชิงลึก
ฟังก์ชัน `tmpfile()` อยู่ใน PHP ตั้งแต่ยุคแรกๆ มันจัดการการสร้างไฟล์และการล้างข้อมูลให้คุณ, โดยหลีกเลี่ยงพฤติกรรมที่อาจส่งผลเสียต่อความปลอดภัยจากการที่ข้อมูลลับต่างๆ ถูกทิ้งไว้

ในทางตรงกันข้าม, `tempnam()` ให้เพียงแค่ชื่อ, ทิ้งการจัดการไฟล์ไว้ในมือของคุณ ประเด็นสำคัญ: อย่าลืม `unlink()` ไฟล์เมื่อคุณเสร็จ

ไฟล์ชั่วคราวเหล่านี้โดยปกติจะถูกเก็บไว้ในไดเรกทอรี temp ของระบบที่เริ่มต้น, ซึ่งคุณสามารถหาได้โดยใช้ `sys_get_temp_dir()`. ตำแหน่งนี้อาจแตกต่างกันไปตามระบบปฏิบัติการและการกำหนดค่าของสภาพแวดล้อมของคุณ

คุณยังมีทางเลือกอื่นอีก เช่น `tempnam()` และ `tmpfile()`, และยังมี `sys_get_temp_dir()` สำหรับการค้นหาไดเรกทอรี temp ที่ยากที่จะค้นหา แต่จำกฎทองของไฟล์ชั่วคราว: จัดการให้เรียบร้อยหลังจากใช้—PHP ทำส่วนนี้ให้อัตโนมัติ, แต่เป็นสิ่งที่ดีที่จะปฏิบัติอย่างชัดเจน

## ดูเพิ่มเติมที่
- [เอกสารการใช้งาน PHP อย่างเป็นทางการสำหรับ tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [คู่มือ PHP เกี่ยวกับฟังก์ชัน tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [ข้อมูลของ PHP.net บน sys_get_temp_dir()](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [ความปลอดภัยของระบบไฟล์](https://www.php.net/manual/en/security.filesystem.php)

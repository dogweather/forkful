---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:51.421651-06:00
description: "\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 PHP \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1B\
  \u0E34\u0E14\u0E44\u0E1F\u0E25\u0E4C\u0E41\u0E25\u0E30\u0E41\u0E17\u0E23\u0E01\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E25\u0E07\u0E44\u0E1B \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E31\u0E01\u0E29\u0E32\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E40\u0E0A\u0E48\u0E19 \u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32\u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E14\u0E22\
  \u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E2B\u0E23\u0E37\u0E2D\u0E1A\u0E31\u0E19\u0E17\
  \u0E36\u0E01\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.330373-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 PHP \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1B\
  \u0E34\u0E14\u0E44\u0E1F\u0E25\u0E4C\u0E41\u0E25\u0E30\u0E41\u0E17\u0E23\u0E01\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E25\u0E07\u0E44\u0E1B \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E31\u0E01\u0E29\u0E32\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E40\u0E0A\u0E48\u0E19 \u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32\u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E14\u0E22\
  \u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E2B\u0E23\u0E37\u0E2D\u0E1A\u0E31\u0E19\u0E17\
  \u0E36\u0E01\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
เขียนไฟล์ข้อความใน PHP หมายถึงการสร้างหรือเปิดไฟล์และแทรกเนื้อหาลงไป โปรแกรมเมอร์ทำเช่นนี้เพื่อรักษาข้อมูล เช่น เนื้อหาที่สร้างโดยผู้ใช้หรือบันทึกการทำงาน ไว้นอกจากวงจรชีวิตของโปรแกรม

## วิธีทำ:
PHP รองรับการเขียนไฟล์โดยธรรมชาติผ่านฟังก์ชันเช่น `file_put_contents`, `fopen` ร่วมกับ `fwrite` และ `fclose` นี่คือวิธีใช้งาน:

### เขียนอย่างง่ายด้วย `file_put_contents`:
ฟังก์ชันนี้ทำให้กระบวนการเขียนไฟล์ง่ายขึ้นโดยการทำทุกอย่างในขั้นตอนเดียว
```php
$content = "Hello, world!";
file_put_contents("hello.txt", $content);
// ตรวจสอบว่าไฟล์ถูกเขียนสำเร็จหรือไม่
if (file_exists("hello.txt")) {
    echo "File created successfully!";
} else {
    echo "Failed to create the file.";
}
```

### เขียนขั้นสูงด้วย `fopen`, `fwrite` และ `fclose`:
สำหรับการควบคุมการเขียนไฟล์ที่มากขึ้น เช่น เพิ่มข้อความหรือการจัดการข้อผิดพลาดมากขึ้น ให้ใช้ `fopen` กับ `fwrite`
```php
$file = fopen("hello.txt", "a"); // 'a' โหมดสำหรับการเพิ่ม, 'w' สำหรับการเขียน
if ($file) {
    fwrite($file, "\nAdding more content.");
    fclose($file);
    echo "Content added successfully!";
} else {
    echo "Failed to open the file.";
}
```

#### อ่านไฟล์เพื่อแสดงผล:
เพื่อตรวจสอบเนื้อหาของเรา:
```php
echo file_get_contents("hello.txt");
```
**ตัวอย่างผลลัพธ์:**
```
Hello, world!
Adding more content.
```

### การใช้ไลบรารีจากบุคคลที่สาม:
สำหรับการดำเนินการกับไฟล์ที่ซับซ้อนกว่า สามารถใช้ไลบรารีเช่น `League\Flysystem` สำหรับชั้นความแตกต่างเหนือระบบไฟล์ แต่ฟังก์ชันภายในของ PHP มักเพียงพอสำหรับงานเขียนไฟล์พื้นฐาน ต่อไปนี้คือตัวอย่างสั้นๆ หากคุณเลือกที่จะสำรวจ `Flysystem`:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "Using Flysystem to write this.");
```
ตัวอย่างนี้สมมติว่าคุณได้ติดตั้ง `league/flysystem` ผ่าน Composer ไลบรารีจากบุคคลที่สามสามารถลดความซับซ้อนในการจัดการไฟล์ที่ซับซ้อนได้มาก โดยเฉพาะเมื่อทำงานกับระบบจัดเก็บข้อมูลต่างๆ อย่างไม่มีปัญหา

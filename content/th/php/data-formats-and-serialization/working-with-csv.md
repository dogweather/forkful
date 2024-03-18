---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:35.480634-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV\
  \ (Comma-Separated Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \ CSV, \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E15\
  \u0E32\u0E23\u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E18\u0E23\u0E23\u0E21\u0E14\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.334342-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV (Comma-Separated\
  \ Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2D\u0E48\
  \u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C CSV, \u0E0B\u0E36\
  \u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E22\u0E2D\
  \u0E14\u0E19\u0E34\u0E22\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E15\u0E32\u0E23\u0E32\
  \u0E07\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E18\u0E23\u0E23\
  \u0E21\u0E14\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?

การทำงานกับ CSV (Comma-Separated Values) หมายถึงการอ่านและเขียนข้อมูลไปยังไฟล์ CSV, ซึ่งเป็นรูปแบบยอดนิยมสำหรับการแสดงข้อมูลตารางในตัวอักษรธรรมดา โปรแกรมเมอร์ทำการนี้เพื่อการแลกเปลี่ยนข้อมูลระหว่างโปรแกรม, ระบบ, หรือฐานข้อมูลต่างๆ ได้อย่างง่ายดาย ด้วยความเรียบง่ายและการรองรับอย่างกว้างขวางของมันในแพลตฟอร์มและภาษาการเขียนโปรแกรมต่างๆ

## วิธีการ:

PHP มีฟังก์ชันต่างๆ ในตัวสำหรับการจัดการกับไฟล์ CSV, ทำให้การอ่านและเขียนไฟล์เหล่านี้เป็นเรื่องง่ายโดยไม่จำเป็นต้องใช้ไลบรารีของบุคคลที่สาม นี่คือตัวอย่างเบื้องต้นในการเริ่มต้น:

### การอ่านไฟล์ CSV

คุณสามารถเปิดไฟล์ CSV และอ่านเนื้อหาของมันโดยใช้ `fopen()` ร่วมกับ `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "จำนวนฟิลด์ในบรรทัด: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

สคริปต์นี้จะพิมพ์จำนวนฟิลด์ในแต่ละบรรทัดตามด้วยเนื้อหาของแต่ละฟิลด์

### การเขียนลงไฟล์ CSV

เพื่อเขียนข้อมูลลงไฟล์ CSV, ใช้ `fopen()` ในโหมดเขียน (`w`) และ `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Name', 'Email'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

สคริปต์นี้สร้างไฟล์ชื่อ `users.csv` และเขียนหัวข้อและสองแถวข้อมูลลงไปในนั้น

### การใช้ไลบรารี: League\Csv

สำหรับการจัดการ CSV ที่ซับซ้อนกว่า, `League\Csv` ไลบรารีเสนอชุดคุณลักษณะที่มั่นคง หลังจากติดตั้งมันผ่าน Composer (`composer require league/csv`), คุณสามารถใช้มันอ่านและเขียนข้อมูล CSV ได้อย่างยืดหยุ่นมากขึ้น

#### การอ่านด้วย League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // ตั้งค่าหากคุณต้องการใช้บรรทัดแรกเป็นส่วนหัว

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

สคริปต์นี้อ่าน `data.csv`, ถือว่าแถวแรกเป็นส่วนหัวของคอลัมน์และพิมพ์แต่ละแถวเป็นอาร์เรย์แบบเชื่อมโยง

#### เขียนด้วย League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Name', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "เขียนลง users_new.csv สำเร็จแล้ว.";
?>
```

นี่สร้าง `users_new.csv` และเขียนหัวข้อแถวตามด้วยสองแถวข้อมูล

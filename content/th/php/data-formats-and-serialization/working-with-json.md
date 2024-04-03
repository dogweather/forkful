---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:53.072683-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E14\
  \u0E33\u0E40\u0E19\u0E34\u0E19\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON \u0E43\u0E19\
  \ PHP \u0E19\u0E31\u0E49\u0E19\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\
  \u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\
  \u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2D\u0E07\u0E04\u0E37\u0E2D `json_encode()`\
  \ \u0E41\u0E25\u0E30 `json_decode()`\u2026"
lastmod: '2024-03-17T21:57:56.333388-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E07\u0E32\u0E19\
  \u0E01\u0E31\u0E1A JSON \u0E43\u0E19 PHP \u0E19\u0E31\u0E49\u0E19\u0E15\u0E23\u0E07\
  \u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\
  \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2D\u0E07\
  \u0E04\u0E37\u0E2D `json_encode()` \u0E41\u0E25\u0E30 `json_decode()` \u0E14\u0E49\
  \u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2D\u0E32\u0E23\
  \u0E4C\u0E40\u0E23\u0E22\u0E4C PHP \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07 JSON \u0E41\u0E25\u0E30\u0E43\u0E19\u0E17\u0E32\u0E07\u0E01\u0E25\u0E31\u0E1A\
  \u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
การดำเนินงานกับ JSON ใน PHP นั้นตรงไปตรงมาด้วยการใช้ฟังก์ชันที่มีอยู่ในตัวเองคือ `json_encode()` และ `json_decode()` ด้านล่างนี้เป็นตัวอย่างที่แสดงวิธีการแปลงอาร์เรย์ PHP เป็นสตริง JSON และในทางกลับกัน:

### การเข้ารหัสอาร์เรย์ PHP เป็นสตริง JSON
```php
// กำหนดอาร์เรย์แบบ associative
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// แปลงอาร์เรย์ PHP เป็นสตริง JSON
$jsonString = json_encode($data);

// แสดงสตริง JSON
echo $jsonString;
```
**ตัวอย่างผลลัพธ์:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### การถอดรหัสสตริง JSON เป็นอาร์เรย์ PHP
```php
// สตริง JSON
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// แปลงสตริง JSON เป็นอาร์เรย์ PHP
$data = json_decode($jsonString, true);

// แสดงอาร์เรย์ PHP
print_r($data);
```
**ตัวอย่างผลลัพธ์:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### การทำงานกับไลบรารีภายนอก: GuzzleHttp
สำหรับการจัดการข้อมูล JSON และคำขอเว็บที่ซับซ้อน ไลบรารี PHP ที่ได้รับความนิยมคือ GuzzleHttp มันทำให้การร้องขอ HTTP เป็นเรื่องง่ายและสามารถทำงานกับข้อมูล JSON ได้ง่าย

**การติดตั้งผ่าน Composer:**
```
composer require guzzlehttp/guzzle
```

**ตัวอย่างการร้องขอ:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// สร้างคำขอไปยัง API ที่ส่งคืนข้อมูล JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// ถอดรหัสการตอบกลับ JSON เป็นอาร์เรย์ PHP
$data = json_decode($response->getBody(), true);

// แสดงข้อมูล
print_r($data);
```

**ถ้าสมมติว่า API ส่งคืนข้อมูล JSON ที่คล้ายกัน:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
นี่แสดงให้เห็นถึงความง่ายในการใช้ PHP สำหรับการจัดการ JSON ทั้งด้วยฟังก์ชันพื้นฐานและด้วยไลบรารีที่มีความสามารถเช่น GuzzleHttp สำหรับงานที่ซับซ้อนยิ่งขึ้น

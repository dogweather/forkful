---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:19.299269-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19 (basic authentication) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\
  \u0E43\u0E0A\u0E49\u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E17\u0E23\
  \u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23\u0E1A\u0E19\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\
  \u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.311869-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19 (basic authentication) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\
  \u0E43\u0E0A\u0E49\u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E17\u0E23\
  \u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23\u0E1A\u0E19\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\
  \u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## การและเหตุผล

การส่งคำขอ HTTP พร้อมกับการตรวจสอบสิทธิ์พื้นฐาน (basic authentication) หมายถึงการเพิ่มชื่อผู้ใช้และรหัสผ่านเพื่อเข้าถึงทรัพยากรบนเซิร์ฟเวอร์ โปรแกรมเมอร์ใช้วิธีนี้เพราะบาง API และเว็บเซอร์วิสต้องการการตรวจสอบสิทธิ์เพื่อให้แน่ใจว่ามีเพียงผู้ใช้ที่ได้รับอนุญาตเท่านั้นที่เข้าถึงข้อมูลของพวกเขา

## วิธีทำ:

นี่คือวิธีการง่ายๆ ในการส่งคำขอ HTTP พร้อมกับการตรวจสอบสิทธิ์พื้นฐานโดยใช้ cURL ใน PHP:

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'your_username';
$password = 'your_password';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

ตัวอย่างผลลัพธ์:

``` 
{
  "authenticated": true,
  "data": "ข้อมูลที่เป็นความลับบางอย่าง"
}
```

## ศึกษาลึก

การตรวจสอบสิทธิ์แบบ HTTP Basic ได้ถูกใช้มาตั้งแต่ช่วงต้นของเว็บ มันไม่ใช่ตัวเลือกที่มีความปลอดภัยที่สุดรอบตัว (เนื่องจากรหัสผ่านถูกส่งในรูปแบบการเข้ารหัส base64 ซึ่งสามารถถอดรหัสได้ง่าย) แต่มันเป็นวิธีที่ง่ายและรวดเร็วในการนำไปใช้สำหรับการควบคุมการเข้าถึงอย่างรวดเร็ว

หากความปลอดภัยเป็นสิ่งที่น่ากังวล (และมันควรเป็นเช่นนั้น) คุณจะหันไปใช้วิธีที่แข็งแกร่งกว่า เช่น OAuth, JWT หรือ API keys อย่างไรก็ตาม การตรวจสอบสิทธิ์พื้นฐานยังคงมีการใช้งานอยู่บางส่วนเนื่องจากระบบเก่า และบางส่วนเนื่องจากระบบภายในที่คุณควบคุมการเข้าถึงได้อย่างแน่นหนา

ใน PHP, cURL ถูกใช้กันอย่างแพร่หลายสำหรับการทำคำขอ HTTP แต่มีทางเลือกอื่น เช่น `file_get_contents` หรือ Guzzle (ลูกค้า HTTP สำหรับ PHP) อยู่ หากใช้ `file_get_contents` ต้องสร้าง context พร้อมกับส่วนหัวที่เหมาะสม:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

การเลือกเครื่องมือที่เหมาะสมขึ้นอยู่กับความต้องการของโปรเจคของคุณและระดับของการควบคุมและฟังก์ชั่นที่คุณต้องการ

## ดูเพิ่มเติม

เพื่อศึกษาลึกขึ้นและขยายความรู้ของคุณ สำรวจสิ่งเหล่านี้:

- [เอกสารคู่มือ cURL](https://www.php.net/manual/en/book.curl.php)
- [เอกสารคู่มือ Guzzle](http://docs.guzzlephp.org/en/stable/)
- [ฟังก์ชั่น PHP `file_get_contents`](https://www.php.net/manual/en/function.file-get-contents.php)
- [การตรวจสอบสิทธิ์ HTTP ด้วย PHP](https://www.php.net/manual/en/features.http-auth.php)

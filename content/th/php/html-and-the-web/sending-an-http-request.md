---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:06.322808-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\
  \u0E1A\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E27\
  \u0E34\u0E2A, API \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\u0E04\
  \u0E48\u0E40\u0E23\u0E35\u0E22\u0E01\u0E14\u0E39\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\
  \u0E32\u0E02\u0E2D\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08"
lastmod: '2024-03-17T21:57:56.308589-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\
  \u0E1A\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E27\
  \u0E34\u0E2A, API \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\u0E04\
  \u0E48\u0E40\u0E23\u0E35\u0E22\u0E01\u0E14\u0E39\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\
  \u0E32\u0E02\u0E2D\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08"
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การส่งคำขอ HTTP คือกระบวนการที่โปรแกรมขอข้อมูลจากเซิร์ฟเวอร์ นักพัฒนาทำเช่นนี้เพื่อโต้ตอบกับเว็บเซอร์วิส, API หรือเพียงแค่เรียกดูเนื้อหาของเว็บเพจ

## วิธีการ:

PHP มีวิธีการที่ดีในการจัดการคำขอ HTTP ด้วยไลบรารี `cURL` แต่ตัวเลือกใหม่คือการใช้ `file_get_contents` สำหรับคำขอ GET ที่ง่ายกว่า หรือ `stream_context_create` สำหรับคำขอ POST นี่คือการดูอย่างรวดเร็วทั้งสองวิธี

### คำขอ GET ด้วย file_get_contents():
```php
// ลิงก์ URL ที่คุณต้องการ
$url = "http://example.com/api";

// ใช้ file_get_contents เพื่อทำคำขอ GET
$response = file_get_contents($url);

// ดัมพ์ข้อมูลสำหรับดูว่าคุณได้รับอะไร
var_dump($response);
```

### คำขอ POST ด้วย stream_context_create():
```php
// ลิงก์ URL ที่คุณกำลังโพสต์ไป
$url = "http://example.com/api";

// ข้อมูลที่คุณกำลังส่ง
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// ตัวเลือกของ stream context
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// สร้าง context ของ stream
$context  = stream_context_create($options);

// ทำการคำขอ POST และเก็บค่าตอบกลับใส่ในตัวแปร
$result = file_get_contents($url, false, $context);

// ดูว่าคุณได้รับอะไร
var_dump($result);
```

## การศึกษาลึก

ในอดีต, `fsockopen()` เป็นตัวเลือกหลักสำหรับคำขอ HTTP ด้วย PHP มันอาจจะไม่คล่องตัว แต่ก็ทำงานได้ดี ต่อมามี `cURL`, ยังคงทรงพลังและได้รับการใช้งานอย่างแพร่หลาย โดยเฉพาะสำหรับการทำงานที่ซับซ้อน แต่บางครั้งคุณไม่จำเป็นต้องใช้เลื่อยเพื่อตัดเส้นด้าย `file_get_contents()` และ `stream_context_create()` จึงเข้ามามีบทบาท

สิ่งที่สำคัญเกี่ยวกับ `file_get_contents()` คือความเรียบง่ายของมัน เหมาะสำหรับคำขอ GET ที่ง่าย แต่ถ้าคุณต้องการ POST ข้อมูลล่ะ? `stream_context_create()` เข้ามามีบทบาท ตัวนี้ช่วยให้คุณปรับแต่งคำขอ HTTP ของคุณด้วยส่วนหัว, วิธีการ และอื่นๆ

ในหลังฉาก, `file_get_contents()` และ `stream_context_create()` ใช้ stream wrappers ของ PHP ซึ่งแทนที่การทำงานของ socket ระดับต่ำที่จัดการโดย `fsockopen()`

ข้อเสียหนึ่ง? การจัดการข้อผิดพลาดอาจซับซ้อนกว่า หากมีการผิดพลาด, ฟังก์ชันเหล่านี้ไม่ค่อยมีความให้อภัยเหมือนกับ `cURL` หากคุณต้องการข้อมูลตอบกลับอย่างละเอียดหรือมีงาน HTTP ที่ซับซ้อน พิจารณาใช้ `cURL`

## ดูเพิ่มเติม

- เอกสาร PHP อย่างเป็นทางการของ cURL: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- บริบทแบบสตรีมของ PHP: [https://www.php.net/manual/en/context.php](https://www.php.net/manual/en/context.php)
- ตัวเลือกบริบท HTTP: [https://www.php.net/manual/en/context.http.php](https://www.php.net/manual/en/context.http.php)

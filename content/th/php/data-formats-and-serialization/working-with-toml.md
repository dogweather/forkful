---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:12.324735-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E04\u0E25\u0E49\u0E32\u0E22\u0E01\
  \u0E31\u0E1A JSON \u0E2B\u0E23\u0E37\u0E2D YAML \u0E41\u0E15\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\u0E19\u0E38\
  \u0E29\u0E22\u0E4C\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\u0E32 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.335619-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E04\u0E25\u0E49\u0E32\u0E22\u0E01\u0E31\
  \u0E1A JSON \u0E2B\u0E23\u0E37\u0E2D YAML \u0E41\u0E15\u0E48\u0E2D\u0E48\u0E32\u0E19\
  \u0E07\u0E48\u0E32\u0E22\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\u0E19\u0E38\u0E29\
  \u0E22\u0E4C\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C config \u0E40\u0E1E\u0E23\
  \u0E32\u0E30\u0E21\u0E31\u0E19\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\
  \u0E32\u0E41\u0E25\u0E30\u0E41\u0E1B\u0E25\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E42\u0E04\
  \u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\
  \u0E14\u0E49\u0E14\u0E35."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีทำ:
ขั้นแรก ตรวจสอบให้แน่ใจว่าคุณได้ติดตั้งไลบรารี TOML parser เช่น `yosymfony/toml` แล้ว มา parse ไฟล์ TOML กันเถอะ:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

ตัวอย่างผลลัพธ์:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## ศึกษาเพิ่มเติม
TOML เกิดขึ้นในปี 2013 ถูกสร้างขึ้นโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub เป็นทางเลือกที่เป็นมิตรต่อผู้ใช้มากกว่า XML และ JSON สำหรับไฟล์ config แม้ว่า JSON จะง่ายสำหรับเครื่องเข้าใจ แต่โครงสร้างของ TOML ทำให้มันง่ายต่อการอ่านของมนุษย์ โดยไม่มีความซับซ้อนของ YAML

ทางเลือกอื่นของ TOML ประกอบด้วย JSON, YAML และ XML แต่ละอย่างมีจุดแข็งและสถานการณ์การใช้งานที่แตกต่างกัน JSON เป็นที่แพร่หลายและเป็นอิสระจากภาษา; YAML สามารถอ่านได้ง่ายกว่าและสนับสนุนความคิดเห็น ขณะที่ XML มีขนาดใหญ่และได้รับการสนับสนุนอย่างแพร่หลาย

เมื่อใช้งาน TOML ใน PHP คุณกำลังมองหา libraries ที่ parse เนื้อหาเป็น arrays หรือ objects ภายใน PHP `yosymfony/toml` เป็น parser ของ PHP ที่ปฏิบัติตามข้อกำหนด v0.4.0 ของ TOML หากต้องการเป็นปัจจุบันที่สุด ควรตรวจสอบ parser ใหม่ ๆ หรือการอัปเดตที่สนับสนุนเวอร์ชันล่าสุดของ TOML (v1.0.0 ณ การอัปเดตครั้งสุดท้ายของฉัน)

## ดูเพิ่มเติม
- ข้อกำหนด TOML: <https://toml.io/>
- TOML Parser สำหรับ PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- เปรียบเทียบรูปแบบข้อมูล (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- ตัวจัดการแพคเกจ PHP (Composer): <https://getcomposer.org/>

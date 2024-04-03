---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:27.280894-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E40\u0E15\u0E34\u0E1A\u0E42\u0E15\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\
  \u0E19\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E44\u0E21\u0E48\u0E23\u0E2D\u0E07\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25 YAML\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\
  \u0E02\u0E2D\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19 \u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\
  \u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E01\u0E31\u0E1A YAML \u0E43\u0E19 PHP\u2026"
lastmod: '2024-03-17T21:57:56.332263-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E15\u0E34\u0E1A\u0E42\u0E15\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E44\
  \u0E21\u0E48\u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\
  \u0E21\u0E27\u0E25\u0E1C\u0E25 YAML \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\
  \u0E2B\u0E19\u0E36\u0E48\u0E07\u0E02\u0E2D\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 \u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\
  \u0E48\u0E07\u0E48\u0E32\u0E22\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML \u0E43\u0E19 PHP\
  \ \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E2D\u0E21\u0E42\
  \u0E1E\u0E40\u0E19\u0E19\u0E17\u0E4C YAML \u0E02\u0E2D\u0E07 Symfony \u0E2B\u0E23\
  \u0E37\u0E2D\u0E2A\u0E48\u0E27\u0E19\u0E02\u0E22\u0E32\u0E22 `yaml` PECL\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
PHP ในการเติบโตปัจจุบันของมัน ไม่รองรับการประมวลผล YAML เป็นส่วนหนึ่งของไลบรารีมาตรฐาน วิธีที่ง่ายที่สุดในการทำงานกับ YAML ใน PHP คือการใช้คอมโพเนนท์ YAML ของ Symfony หรือส่วนขยาย `yaml` PECL

### การใช้งานคอมโพเนนท์ YAML ของ Symfony
ก่อนอื่น ติดตั้งคอมโพเนนท์ YAML ของ Symfony ผ่าน Composer:

```bash
composer require symfony/yaml
```

จากนั้น คุณสามารถแยกและเก็บ YAML ดังนี้:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// การแยก YAML
$yamlString = <<<YAML
greet: สวัสดี, โลก!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// การสร้าง YAML จากอาร์เรย์
$array = [
    'greet' => 'สวัสดี, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

ผลลัพธ์ตัวอย่างเมื่อแยก:

```
Array
(
    [greet] => สวัสดี, โลก!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

ผลลัพธ์ตัวอย่างเมื่อเก็บ:

```
greet: สวัสดี, YAML!
framework:
    name: Symfony
    language: PHP
```

### การใช้ส่วนขยาย `yaml` PECL
หากคุณต้องการ หรือหากความต้องการของโครงการของคุณอนุญาต ส่วนขยาย PECL สามารถเป็นวิธีอื่นที่มีประสิทธิภาพในการทำงานกับ YAML ก่อนอื่น ตรวจสอบให้แน่ใจว่ามีการติดตั้งส่วนขยายนั้น:

```bash
pecl install yaml
```

จากนั้น เปิดใช้งานในการกำหนดค่า `php.ini` ของคุณ:

```ini
extension=yaml.so
```

นี่คือวิธีการแยกและส่งออก YAML:

```php
<?php

// การแยก YAML
$yamlString = <<<YAML
greet: สวัสดี, โลก!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// การสร้าง YAML จากอาร์เรย์
$array = [
    'greet' => 'สวัสดี, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

ผลลัพธ์จะคล้ายกับส่วนของ Symfony แสดงให้เห็นถึงบทบาทของ YAML เป็นสะพานระหว่างรูปแบบที่อ่านได้โดยมนุษย์และโครงสร้างอาร์เรย์ PHP ทำให้การกำหนดค่าและการจัดการข้อมูลง่ายขึ้น

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:57.917564-06:00
description: "TOML \u0E2B\u0E23\u0E37\u0E2D Tom's Obvious, Minimal Language \u0E40\
  \u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\
  \u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E17\u0E35\u0E48\u0E2D\u0E48\
  \u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\
  \u0E01\u0E21\u0E35\u0E0B\u0E35\u0E21\u0E31\u0E19\u0E15\u0E34\u0E01\u0E2A\u0E4C\u0E17\
  \u0E35\u0E48\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.924494-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E2B\u0E23\u0E37\u0E2D Tom's Obvious, Minimal Language \u0E40\u0E1B\
  \u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\
  \u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\
  \u0E21\u0E35\u0E0B\u0E35\u0E21\u0E31\u0E19\u0E15\u0E34\u0E01\u0E2A\u0E4C\u0E17\u0E35\
  \u0E48\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E41\
  \u0E2D\u0E1E\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E0B\u0E2D\u0E1F\u0E15\
  \u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\u0E07\
  \u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E04\u0E27\
  \u0E32\u0E21\u0E2A\u0E31\u0E1A\u0E2A\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E19\u0E49\u0E2D\u0E22."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?

TOML หรือ Tom's Obvious, Minimal Language เป็นรูปแบบไฟล์การตั้งค่าที่อ่านง่ายเนื่องจากมีซีมันติกส์ที่ชัดเจน โปรแกรมเมอร์ใช้มันสำหรับการตั้งค่าแอพพลิเคชันซอฟต์แวร์เพราะมันง่ายต่อการวิเคราะห์และสร้างความสับสนหรือข้อผิดพลาดน้อย

## วิธีทำ:

Dart ไม่ได้รวมการสนับสนุน TOML อย่างในตัว แต่คุณสามารถทำงานกับไฟล์ TOML โดยใช้แพ็กเกจของบุคคลที่สาม เช่น `toml` ก่อนอื่น เพิ่ม `toml` ลงใน `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  toml: ^0.10.0
```

### อ่าน TOML

ในการอ่านไฟล์ TOML สมมติว่าคุณมีไฟล์การตั้งค่าง่ายๆ `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

คุณสามารถวิเคราะห์ไฟล์ TOML นี้ใน Dart ดังนี้:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // พิมพ์ส่วน 'database'
}
```

ซึ่งจะพิมพ์:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### เขียน TOML

ในการสร้างเนื้อหา TOML ให้ใช้ `TomlBuilder` ที่มีให้ในแพ็กเกจ `toml`:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

สิ่งนี้จะสร้างและพิมพ์สตริงแทนที่เนื้อหา TOML ที่คล้ายกับไฟล์ `config.toml` ของเรามาก:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

ตัวอย่างเหล่านี้แสดงวิธีการอ่านจากและเขียนไปยังไฟล์ TOML ทำให้ง่ายต่อการทำงานกับข้อมูลการตั้งค่าในแอพพลิเคชัน Dart ของคุณ

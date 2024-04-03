---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:25.635161-06:00
description: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\
  \u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\
  \u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32, \u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.921654-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\
  \u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\
  \u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32, \u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\
  \ \u0E41\u0E25\u0E30\u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\
  \u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E15\u0E49\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E2B\u0E23\
  \u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\
  \u0E35\u0E48\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E07\u0E48\u0E32\u0E22."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
ใน Dart, การทำงานกับ YAML โดยทั่วไปจะเกี่ยวข้องกับการใช้ไลบรารีภายนอก เนื่องจากภาษานี้ไม่รวมความสามารถในการแยกวิเคราะห์ YAML เข้ามาด้วย ตัวเลือกที่ได้รับความนิยมคือแพคเกจ `yaml` ในการเริ่มต้น คุณจะต้องเพิ่มแพคเกจนี้ลงใน `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  yaml: ^3.1.0
```

อย่าลืมรัน `pub get` เพื่อดึงแพคเกจ.

### การอ่าน YAML
ในการอ่านไฟล์ YAML ขั้นแรกให้นำเข้าแพคเกจ `yaml` แล้วใช้ฟังก์ชั่น `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // ผลลัพธ์: John Doe
}

```

โดยสมมติว่าไฟล์ `config.yaml` ของคุณมีหน้าตาดังนี้:

```yaml
name: John Doe
age: 30
```

### การเขียน YAML
ในขณะที่แพคเกจ `yaml` เหมาะสำหรับการวิเคราะห์ข้อมูล แต่ไม่รองรับการเขียน YAML สำหรับสิ่งนั้น คุณอาจจำเป็นต้องแปลงข้อมูลของคุณเป็น YAML ด้วยมือหรือใช้แพคเกจอื่นหากมี หรือ วิธีที่ง่ายกว่าคือจัดการการเปลี่ยนแปลงข้อมูลของคุณและแสดงผลออกมาเป็นสตริงที่ตรงกับไวยากรณ์ YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // ผลลัพธ์: name: Jane Doe
                             //         age: 29
}
```

นี่เป็นวิธีการพื้นฐานและอาจไม่เหมาะสมกับโครงสร้างข้อมูลที่ซับซ้อนหรือคุณสมบัติพิเศษของ YAML สำหรับความต้องการที่ซับซ้อนเพิ่มเติม คุณอาจต้องมองหาหรือมีส่วนร่วมในแพคเกจ Dart ที่ครอบคลุมมากขึ้น

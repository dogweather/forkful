---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:02.698601-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Dart \u0E23\u0E27\u0E21\
  \ `http` \u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08 \u0E0B\u0E36\u0E48\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E17\u0E23\u0E07\u0E1E\
  \u0E25\u0E31\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E30\u0E14\u0E27\u0E01\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E17\u0E23\u0E31\u0E1E\
  \u0E22\u0E32\u0E01\u0E23 HTTP \u0E43\u0E19\u0E02\u0E31\u0E49\u0E19\u0E15\u0E49\u0E19\
  \ \u0E43\u0E2B\u0E49\u0E23\u0E27\u0E21\u0E21\u0E31\u0E19\u0E43\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C pubspec.yaml \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
lastmod: '2024-03-17T21:57:55.892874-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0E23\u0E27\u0E21 `http` \u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08\
  \ \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\
  \u0E35\u0E48\u0E17\u0E23\u0E07\u0E1E\u0E25\u0E31\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E30\
  \u0E14\u0E27\u0E01\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\
  \u0E31\u0E1A\u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23 HTTP \u0E43\u0E19\u0E02\
  \u0E31\u0E49\u0E19\u0E15\u0E49\u0E19 \u0E43\u0E2B\u0E49\u0E23\u0E27\u0E21\u0E21\u0E31\
  \u0E19\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C pubspec.yaml \u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
Dart รวม `http` แพ็คเกจ ซึ่งเป็นวิธีที่ทรงพลังและสะดวกในการทำงานกับทรัพยากร HTTP ในขั้นต้น ให้รวมมันในไฟล์ pubspec.yaml ของคุณ:

```yaml
dependencies:
  http: ^0.13.3
```

จากนั้น ให้นำเข้ามันในโค้ด Dart เพื่อเริ่มทำการร้องขอ:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('เนื้อหาการตอบกลับ: ${response.body}');
  } else {
    print('คำขอล้มเหลวด้วยสถานะ: ${response.statusCode}.');
  }
}
```

ตัวอย่างการแสดงผลสำหรับคำขอที่สำเร็จอาจดูเช่นนี้:

```
เนื้อหาการตอบกลับ: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

สำหรับคำขอที่ซับซ้อนกว่า เช่น คำขอ POST ที่มีเนื้อหาเป็น JSON คุณจะทำตามขั้นตอนดังต่อไปนี้:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('สถานะการตอบกลับ: ${response.statusCode}');
    print('เนื้อหาการตอบกลับ: ${response.body}');
  } else {
    print('ล้มเหลวในการสร้างโพสต์ใหม่ สถานะ: ${response.statusCode}');
  }
}
```

ตัวอย่างการแสดงผลสำหรับคำขอโพสต์อาจเป็น:

```
สถานะการตอบกลับ: 201
เนื้อหาการตอบกลับ: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

ตัวอย่างเหล่านี้แสดงคำขอ HTTP GET และ POST พื้นฐานโดยใช้แพ็คเกจ `http` ใน Dart แพ็คเกจนี้ครอบคลุมความต้องการสำหรับการส่งคำขอ HTTP รวมถึงสถานการณ์ที่ซับซ้อนกว่าด้วยส่วนหัวและเนื้อหา.

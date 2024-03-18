---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:28.354528-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21 \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\
  \u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 HTML \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\
  \u0E23\u0E37\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\
  \u0E01\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.893896-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21 \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\
  \u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 HTML \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\
  \u0E23\u0E37\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\
  \u0E01\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การแยกส่วน HTML ในการเขียนโปรแกรม เกี่ยวข้องกับการดึงข้อมูลจากเอกสาร HTML โปรแกรมเมอร์ทำสิ่งนี้เพื่อโต้ตอบหรือดึงข้อมูลจากเนื้อหาเว็บเพื่อการสกัดข้อมูล, การทดสอบ, หรือการทำงานอัตโนมัติ แม้ว่าจะไม่มี API อย่างเป็นทางการก็ตาม

## วิธีการ:
Dart ไม่มีการสนับสนุนในการแยกส่วน HTML ในไลบรารีหลักของมัน อย่างไรก็ตาม, คุณสามารถใช้แพ็คเกจของบุคคลที่สาม เช่น `html` เพื่อแยกส่วนและจัดการเอกสาร HTML

ก่อนอื่น, เพิ่มแพ็คเกจ `html` ไปยังไฟล์ `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  html: ^0.15.0
```

จากนั้น, นำเข้าแพ็คเกจลงในไฟล์ Dart ของคุณ:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

นี่เป็นตัวอย่างพื้นฐานของการแยกส่วนสตริงที่มี HTML และการสกัดข้อมูล:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hello, Dart!</h1>
      <p>This is a paragraph in a sample HTML</p>
    </body>
  </html>
  """;

  // แยกส่วนสตริง HTML
  Document document = parse(htmlDocument);

  // การสกัดข้อมูล
  String title = document.querySelector('h1')?.text ?? "ไม่พบหัวข้อ";
  String paragraph = document.querySelector('p')?.text ?? "ไม่พบย่อหน้า";

  print('หัวข้อ: $title');
  print('ย่อหน้า: $paragraph');
}
```

ผลลัพธ์:

```
หัวข้อ: Hello, Dart!
ย่อหน้า: This is a paragraph in a sample HTML
```

เพื่อโต้ตอบกับหน้าเว็บในโลกจริง, คุณอาจจะรวมการแยกส่วน `html` กับการร้องขอ HTTP (โดยใช้แพ็คเกจ `http` เพื่อดึงเนื้อหาเว็บ) นี่คือตัวอย่างอย่างรวดเร็ว:

ก่อนอื่น, เพิ่มแพ็คเกจ `http` พร้อมกับ `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

จากนั้น, ดึงและแยกส่วนหน้า HTML จากเว็บ:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // ดึงเว็บเพจ
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // สมมติว่าหน้านี้มีแท็ก <h1> ที่คุณสนใจ
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('ข่าวหัวข้อ: $headlines');
  } else {
    print('คำขอล้มเหลวด้วยสถานะ: ${response.statusCode}.');
  }
}
```

หมายเหตุ: เทคนิคการดึงข้อมูลจากเว็บที่แสดงด้านบนควรถูกใช้อย่างรับผิดชอบและตามข้อกำหนดการให้บริการของเว็บไซต์

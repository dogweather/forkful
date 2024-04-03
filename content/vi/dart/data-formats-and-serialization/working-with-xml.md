---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:32.220843-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3\
  \ s\u1EB5n c\xF3 cho vi\u1EC7c x\u1EED l\xFD XML trong th\u01B0 vi\u1EC7n chu\u1EA9\
  n c\u1EE7a n\xF3, y\xEAu c\u1EA7u s\u1EED d\u1EE5ng c\xE1c g\xF3i ph\u1EA7n m\u1EC1\
  m c\u1EE7a b\xEAn th\u1EE9 ba. M\u1ED9t g\xF3i ph\u1ED5\u2026"
lastmod: '2024-03-13T22:44:36.295076-06:00'
model: gpt-4-0125-preview
summary: "Dart kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3 s\u1EB5n c\xF3 cho vi\u1EC7\
  c x\u1EED l\xFD XML trong th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a n\xF3, y\xEAu c\u1EA7\
  u s\u1EED d\u1EE5ng c\xE1c g\xF3i ph\u1EA7n m\u1EC1m c\u1EE7a b\xEAn th\u1EE9 ba."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Làm thế nào:
Dart không bao gồm hỗ trợ sẵn có cho việc xử lý XML trong thư viện chuẩn của nó, yêu cầu sử dụng các gói phần mềm của bên thứ ba. Một gói phổ biến là `xml`. Để sử dụng, bạn trước tiên cần thêm nó vào `pubspec.yaml` của mình:

```yaml
dependencies:
  xml: ^5.0.0 // Sử dụng phiên bản mới nhất có sẵn
```

Sau đó, nhập gói vào tệp Dart của bạn:

```dart
import 'package:xml/xml.dart' as xml;
```

**Phân tích cú pháp XML:**

Giả sử bạn có một chuỗi XML như sau:

```xml
<String name="greeting">Hello, world!</String>
```

Bạn có thể phân tích và đọc XML như sau:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // In ra: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**Tạo Tài liệu XML:**

Việc tạo một tài liệu XML mới là rất dễ dàng với gói `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Kết quả**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**Truy vấn và Chỉnh sửa XML:**

Để tìm hoặc chỉnh sửa các phần tử, bạn có thể sử dụng các phương pháp tương tự như XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Chỉnh sửa thuộc tính 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Thêm một phần tử con mới
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**Kết quả**:

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

Những ví dụ này minh họa các thao tác cơ bản khi làm việc với XML trong Dart. Với gói `xml`, bạn có thể phân tích cú pháp, tạo và thao tác tài liệu XML để đáp ứng yêu cầu của ứng dụng.

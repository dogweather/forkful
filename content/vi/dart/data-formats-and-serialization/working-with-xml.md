---
title:                "Làm việc với XML"
date:                  2024-03-08T21:57:32.220843-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc làm việc với XML trong Dart bao gồm việc phân tích cú pháp, truy vấn và chỉnh sửa các tài liệu XML, một quá trình rất quan trọng đối với các ứng dụng tương tác với các dịch vụ web, các tệp cấu hình hoặc các hệ thống cũ. Lập trình viên làm điều này để kích hoạt trao đổi dữ liệu, cấu hình, hoặc thậm chí là các lời gọi thủ tục từ xa trong một định dạng có cấu trúc, phân cấp, dễ đọc cho con người và có thể phân tích cú pháp bởi máy.

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

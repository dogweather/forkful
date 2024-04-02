---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:24.718062-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong l\u1EADp tr\xECnh bao g\u1ED3\
  m vi\u1EC7c tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u t\u1EEB t\xE0i li\u1EC7u HTML.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1\
  ng t\xE1c v\u1EDBi ho\u1EB7c c\u1EA1o d\u1EEF li\u1EC7u web\u2026"
lastmod: '2024-03-13T22:44:36.259888-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong l\u1EADp tr\xECnh bao g\u1ED3m\
  \ vi\u1EC7c tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u t\u1EEB t\xE0i li\u1EC7u HTML.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1\
  ng t\xE1c v\u1EDBi ho\u1EB7c c\u1EA1o d\u1EEF li\u1EC7u web\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Gì và Tại sao?
Phân tích cú pháp HTML trong lập trình bao gồm việc trích xuất dữ liệu từ tài liệu HTML. Lập trình viên làm điều này để tương tác với hoặc cạo dữ liệu web để trích xuất thông tin, kiểm tra, hoặc tự động hóa, ngay cả khi không có các API chính thức.

## Làm thế nào:
Dart không cung cấp hỗ trợ tích hợp sẵn cho việc phân tích cú pháp HTML trong các thư viện cốt lõi của mình. Tuy nhiên, bạn có thể sử dụng gói bên thứ ba như `html` để phân tích cú pháp và thao tác với tài liệu HTML.

Đầu tiên, thêm gói `html` vào file `pubspec.yaml` của bạn:

```yaml
dependencies:
  html: ^0.15.0
```

Sau đó, nhập gói vào file Dart của bạn:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Dưới đây là một ví dụ cơ bản về việc phân tích cú pháp một chuỗi chứa HTML và trích xuất dữ liệu:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Xin chào, Dart!</h1>
      <p>Đây là một đoạn văn trong HTML mẫu</p>
    </body>
  </html>
  """;

  // Phân tích cú pháp chuỗi HTML
  Document document = parse(htmlDocument);

  // Trích xuất dữ liệu
  String title = document.querySelector('h1')?.text ?? "Không tìm thấy tiêu đề";
  String paragraph = document.querySelector('p')?.text ?? "Không tìm thấy đoạn văn";

  print('Tiêu đề: $title');
  print('Đoạn văn: $paragraph');
}
```

Kết quả:

```
Tiêu đề: Xin chào, Dart!
Đoạn văn: Đây là một đoạn văn trong HTML mẫu
```

Để tương tác với các trang web thực tế, bạn có thể kết hợp phân tích cú pháp `html` với các yêu cầu HTTP (sử dụng gói `http` để tải nội dung web). Dưới đây là một ví dụ nhanh:

Đầu tiên, thêm gói `http` cùng với `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Sau đó, tải và phân tích cú pháp một trang HTML từ web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Tải trang web
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Giả định trang có các thẻ <h1> mà bạn quan tâm
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Tiêu đề: $headlines');
  } else {
    print('Yêu cầu thất bại với trạng thái: ${response.statusCode}.');
  }
}
```

Lưu ý: Kỹ thuật cào dữ liệu web được trình bày ở trên nên được sử dụng một cách có trách nhiệm và tuân thủ điều khoản dịch vụ của trang web.

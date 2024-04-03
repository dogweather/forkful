---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.796442-07:00
description: "C\xE1ch l\xE0m: Dart cung c\u1EA5p c\xFA ph\xE1p \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 t\u1EA1o v\xE0 thao t\xE1c v\u1EDBi Maps. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 c\xE1c v\xED d\u1EE5 minh h\u1ECDa c\xE1c thao t\xE1c c\u01A1 b\u1EA3n nh\u01B0\
  \ t\u1EA1o, th\xEAm ph\u1EA7n t\u1EED v\xE0 truy\u2026"
lastmod: '2024-03-13T22:44:36.253294-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p c\xFA ph\xE1p \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 t\u1EA1\
  o v\xE0 thao t\xE1c v\u1EDBi Maps."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Cách làm:
Dart cung cấp cú pháp đơn giản để tạo và thao tác với Maps. Dưới đây là các ví dụ minh họa các thao tác cơ bản như tạo, thêm phần tử và truy xuất giá trị.

```dart
void main() {
  // Tạo map
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // Thêm cặp khóa-giá trị mới
  fruitColors['orange'] = 'orange';

  // Truy cập giá trị thông qua khóa của nó
  print(fruitColors['apple']); // Đầu ra: red

  // Cập nhật giá trị
  fruitColors['banana'] = 'green';

  // Duyệt qua Map
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // Mẫu Đầu Ra:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

Đối với cấu trúc dữ liệu phức tạp hoặc chức năng mở rộng, các lập trình viên Dart thường dựa vào các thư viện bổ sung. Một thư viện như vậy là `collection`, cung cấp các loại tập hợp nâng cao và tiện ích. Mặc dù `collection` không thay đổi cách cơ bản Maps được xử lý, nó làm giàu chúng bằng các chức năng tiện ích và các loại tập hợp phức tạp hơn. Dưới đây là cách bạn có thể sử dụng nó cho một nhiệm vụ cụ thể hơn, như sắp xếp Map theo giá trị của nó:

Trước tiên, đảm bảo gói `collection` được bao gồm trong tệp `pubspec.yaml` của bạn:

```yaml
dependencies:
  collection: ^1.15.0
```

Sau đó, bạn có thể sử dụng nó như sau:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // Sắp xếp Map theo giá trị của nó (màu sắc)
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // Đầu ra:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

Ví dụ này minh họa việc sắp xếp các mục nhập của Map dựa trên giá trị của chúng, cho thấy cách Dart và hệ sinh thái phong phú của nó có thể linh hoạt xử lý mảng kết hợp cho việc thao tác dữ liệu phức tạp hơn.

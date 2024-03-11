---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.796442-07:00
description: "M\u1EA3ng k\u1EBFt h\u1EE3p trong Dart, th\u01B0\u1EDDng \u0111\u01B0\
  \u1EE3c bi\u1EBFt \u0111\u1EBFn v\u1EDBi t\xEAn g\u1ECDi Maps, l\xE0 c\u1EA5u tr\xFA\
  c d\u1EEF li\u1EC7u l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng\
  \ c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. Ch\xFAng cho ph\xE9p l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-11T00:14:09.506263-06:00'
model: gpt-4-0125-preview
summary: "M\u1EA3ng k\u1EBFt h\u1EE3p trong Dart, th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c bi\u1EBFt \u0111\u1EBFn v\u1EDBi t\xEAn g\u1ECDi Maps, l\xE0 c\u1EA5u tr\xFAc\
  \ d\u1EEF li\u1EC7u l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng\
  \ c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. Ch\xFAng cho ph\xE9p l\u1EADp tr\xECnh\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp trong Dart, thường được biết đến với tên gọi Maps, là cấu trúc dữ liệu lưu trữ dữ liệu dưới dạng cặp khóa-giá trị. Chúng cho phép lập trình viên truy cập phần tử không qua chỉ mục, mà qua khóa, làm cho việc truy xuất dữ liệu trở nên trực quan và hiệu quả, đặc biệt khi làm việc với dữ liệu có cấu trúc mà mỗi phần tử đều có một định danh duy nhất.

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

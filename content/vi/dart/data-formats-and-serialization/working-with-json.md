---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:23.961653-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) bao g\u1ED3\
  m vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p d\u1EEF li\u1EC7u JSON t\u1EEB chu\u1ED7\
  i th\xE0nh c\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng Dart v\xE0 ng\u01B0\u1EE3c l\u1EA1\
  i, m\u1ED9t nhi\u1EC7m v\u1EE5\u2026"
lastmod: '2024-03-09T21:06:01.025420-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) bao g\u1ED3\
  m vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p d\u1EEF li\u1EC7u JSON t\u1EEB chu\u1ED7\
  i th\xE0nh c\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng Dart v\xE0 ng\u01B0\u1EE3c l\u1EA1\
  i, m\u1ED9t nhi\u1EC7m v\u1EE5\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Làm việc với JSON (JavaScript Object Notation) bao gồm việc phân tích cú pháp dữ liệu JSON từ chuỗi thành các đối tượng Dart và ngược lại, một nhiệm vụ phổ biến trong phát triển web và ứng dụng để trao đổi dữ liệu. Các lập trình viên làm điều này để xử lý dữ liệu một cách hiệu quả từ các API, cấu hình, hoặc giao tiếp giữa các thành phần trong ứng dụng của họ.

## Làm thế nào:

Dart cung cấp hỗ trợ sẵn có cho JSON với thư viện `dart:convert`, làm cho việc mã hóa và giải mã JSON trở nên dễ dàng. Dưới đây là các ví dụ minh họa các thao tác cơ bản:

**Phân tích cú pháp Chuỗi JSON thành Đối tượng Dart:**
```dart
import 'dart:convert';

void main() {
  // Chuỗi JSON ví dụ
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Giải mã JSON thành Bản đồ Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Xin chào, ${user['name']}! Bạn ${user['age']} tuổi.');
  // Kết quả: Xin chào, John! Bạn 30 tuổi.
}
```

**Mã hóa Đối tượng Dart thành Chuỗi JSON:**
```dart
import 'dart:convert';

void main() {
  // Đối tượng Dart ví dụ
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Mã hóa Bản đồ Dart thành JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Kết quả: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Sử dụng `json_serializable` cho các Mô hình Phức tạp:**
Đối với các mô hình dữ liệu phức tạp, việc tuần tự hóa thủ công có thể gây rườm rà. Gói `json_serializable` tự động hóa quá trình này. Nó đòi hỏi thiết lập bổ sung, bao gồm thêm các phụ thuộc vào `pubspec.yaml` và tạo các tệp build. Sau khi thiết lập, bạn có thể sử dụng nó như sau:

1. Định nghĩa một mô hình với các chú thích:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. Tạo boilerplate cho tuần tự hóa:
Sử dụng lệnh build runner để tạo tệp `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Sử dụng mô hình của bạn:
```dart
void main() {
  // Phân tích cú pháp JSON thành User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Người dùng: ${user.name}, Tuổi: ${user.age}');
  // Kết quả: Người dùng: John, Tuổi: 30

  // Chuyển ngược User thành JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Kết quả: {"name":"John","age":30,"email":"john@example.com"}
}
```

Các ví dụ này minh họa các tương tác JSON cơ bản và nâng cao trong Dart, mang đến cho các nhà phát triển khả năng xử lý các nhiệm vụ tuần tự hóa dữ liệu trong ứng dụng của họ một cách mượt mà.

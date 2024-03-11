---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.661892-07:00
description: "T\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Dart li\xEAn quan \u0111\u1EBF\
  n vi\u1EC7c t\u1EA1o c\xE1c gi\xE1 tr\u1ECB s\u1ED1 m\xE0 kh\xF4ng th\u1EC3 d\u1EF1\
  \ \u0111o\xE1n tr\u01B0\u1EDBc v\xE0 kh\xE1c nhau m\u1ED7i khi th\u1EF1c thi. L\u1EAD\
  p tr\xECnh vi\xEAn t\u1EADn d\u1EE5ng ch\u1EE9c\u2026"
lastmod: '2024-03-11T00:14:09.510446-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Dart li\xEAn quan \u0111\u1EBF\
  n vi\u1EC7c t\u1EA1o c\xE1c gi\xE1 tr\u1ECB s\u1ED1 m\xE0 kh\xF4ng th\u1EC3 d\u1EF1\
  \ \u0111o\xE1n tr\u01B0\u1EDBc v\xE0 kh\xE1c nhau m\u1ED7i khi th\u1EF1c thi. L\u1EAD\
  p tr\xECnh vi\xEAn t\u1EADn d\u1EE5ng ch\u1EE9c\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo số ngẫu nhiên trong Dart liên quan đến việc tạo các giá trị số mà không thể dự đoán trước và khác nhau mỗi khi thực thi. Lập trình viên tận dụng chức năng này cho nhiều mục đích, từ mô phỏng các tình huống thế giới thực trong môi trường kiểm tra đến việc kích hoạt cơ chế trò chơi và đảm bảo an ninh thông qua sự ngẫu nhiên trong các hoạt động mã hóa.

## Làm thế nào:

Thư viện cốt lõi của Dart bao gồm hỗ trợ để tạo số ngẫu nhiên với lớp `Random` được tìm thấy trong `dart:math`. Dưới đây là một ví dụ cơ bản:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Tạo một số nguyên ngẫu nhiên từ 0 đến 99
  double randomDouble = rand.nextDouble(); // Tạo một số double ngẫu nhiên từ 0.0 đến 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Kết quả mẫu: (Điều này sẽ thay đổi mỗi khi nó được chạy)*

```
23
0.6722390975465775
```

Đối với các trường hợp sử dụng cần đặc điểm ngẫu nhiên mật mã, Dart cung cấp constructor `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Kết quả mẫu: (Điều này sẽ thay đổi mỗi khi nó được chạy)*

```
45
```

Nếu bạn đang làm việc trên các dự án Flutter hoặc cần ngẫu nhiên phức tạp hơn, bạn có thể thấy gói `faker` hữu ích cho việc tạo ra một loạt dữ liệu ngẫu nhiên, như tên, địa chỉ, và ngày tháng.

Để sử dụng `faker`, trước tiên, thêm nó vào file `pubspec.yaml` của bạn:

```yaml
dependencies:
  faker: ^2.0.0
```

Sau đó, nhập và sử dụng như được hiển thị:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Tạo một tên ngẫu nhiên
  print(faker.address.city()); // Tạo một tên thành phố ngẫu nhiên
}
```

*Kết quả mẫu:*

```
Josie Runolfsdottir
East Lysanne
```

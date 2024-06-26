---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.661892-07:00
description: "L\xE0m th\u1EBF n\xE0o: Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7a\
  \ Dart bao g\u1ED3m h\u1ED7 tr\u1EE3 \u0111\u1EC3 t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEA\
  n v\u1EDBi l\u1EDBp `Random` \u0111\u01B0\u1EE3c t\xECm th\u1EA5y trong `dart:math`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.257292-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7a Dart bao g\u1ED3m h\u1ED7 tr\u1EE3\
  \ \u0111\u1EC3 t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn v\u1EDBi l\u1EDBp `Random` \u0111\
  \u01B0\u1EE3c t\xECm th\u1EA5y trong `dart:math`."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

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

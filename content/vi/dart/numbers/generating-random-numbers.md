---
title:                "Sinh số ngẫu nhiên"
date:                  2024-03-08T21:54:58.661892-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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

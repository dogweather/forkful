---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:13.700765-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ch\xEDnh Dart kh\xF4ng bao g\u1ED3m m\u1ED9\
  t th\u01B0 vi\u1EC7n t\xEDch h\u1EE3p s\u1EB5n cho s\u1ED1 ph\u1EE9c, \u0111\xF2\
  i h\u1ECFi vi\u1EC7c tri\u1EC3n khai m\u1ED9t l\u1EDBp s\u1ED1 ph\u1EE9c t\xF9y\
  \ ch\u1EC9nh ho\u1EB7c s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.254626-06:00'
model: gpt-4-0125-preview
summary: "Ch\xEDnh Dart kh\xF4ng bao g\u1ED3m m\u1ED9t th\u01B0 vi\u1EC7n t\xEDch\
  \ h\u1EE3p s\u1EB5n cho s\u1ED1 ph\u1EE9c, \u0111\xF2i h\u1ECFi vi\u1EC7c tri\u1EC3\
  n khai m\u1ED9t l\u1EDBp s\u1ED1 ph\u1EE9c t\xF9y ch\u1EC9nh ho\u1EB7c s\u1EED d\u1EE5\
  ng m\u1ED9t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9 ba."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Chính Dart không bao gồm một thư viện tích hợp sẵn cho số phức, đòi hỏi việc triển khai một lớp số phức tùy chỉnh hoặc sử dụng một thư viện bên thứ ba. Một lựa chọn phổ biến cho các tác vụ tính toán khoa học, bao gồm hỗ trợ cho số phức, là `package:scidart`.

### Triển khai Lớp Số Phức Cơ Bản
Đối với các thao tác đơn giản, bạn có thể dễ dàng định nghĩa lớp số phức của riêng mình:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Cộng hai số phức
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Biểu diễn chuỗi để dễ dàng gỡ lỗi
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### Sử dụng SciDart cho Các Thao Tác Phức Tạp
Đối với các thao tác phức tạp hơn hoặc khi hiệu năng là quan trọng, `package:scidart` cung cấp hỗ trợ đầy đủ cho số phức cùng với các chức năng tính toán khoa học khác. Đầu tiên, thêm SciDart vào pubspec.yaml của bạn:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Dưới đây là cách thực hiện các thao tác cơ bản với số phức sử dụng SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Tạo số phức
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Phép cộng
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Phép nhân
  var product = complexMultiply(complexNum1, complexNum2);

  print('Tổng: ${sum.toString()}');  // Tổng: Complex(real: 7.0, imaginary: 10.0)
  print('Tích: ${product.toString()}');  // Tích: Complex(real: -11.0, imaginary: 41.0)
}
```

Những ví dụ này minh hoạ việc thao tác và sử dụng cơ bản số phức trong Dart, cả thông qua triển khai tùy chỉnh và qua thư viện SciDart, làm nổi bật sự linh hoạt và sức mạnh của Dart cho các tác vụ tính toán khoa học.

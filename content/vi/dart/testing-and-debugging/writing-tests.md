---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:02.116928-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Dart, g\xF3i `test` th\u01B0\u1EDDng \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 vi\u1EBFt ki\u1EC3m tra. \u0110\u1EA7\
  u ti\xEAn, th\xEAm g\xF3i `test` v\xE0o `pubspec.yaml` c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.267675-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, g\xF3i `test` th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng \u0111\u1EC3 vi\u1EBFt ki\u1EC3m tra."
title: "Vi\u1EBFt b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm thế nào:
Trong Dart, gói `test` thường được sử dụng để viết kiểm tra. Đầu tiên, thêm gói `test` vào `pubspec.yaml` của bạn:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Sau đó, viết một kiểm tra cho một hàm đơn giản. Giả sử bạn có một hàm cộng hai số:

```dart
int add(int a, int b) {
  return a + b;
}
```

Tiếp theo, tạo một tệp tên `add_test.dart` trong thư mục `test` và viết trường hợp kiểm tra của bạn:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Giả sử hàm `add` của bạn nằm trong lib/add.dart

void main() {
  test('cộng hai số', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

Để chạy các bài kiểm tra, sử dụng lệnh Dart:

```bash
$ dart test
```

Kết quả mẫu có thể giống như:

```
00:01 +1: Tất cả các bài kiểm tra đã vượt qua!
```

### Sử dụng một thư viện bên thứ ba: Mockito để mô phỏng (mocking)
Đối với việc kiểm tra mã có các phụ thuộc phức tạp, bạn có thể sử dụng Mockito để tạo các đối tượng mô phỏng. Đầu tiên, thêm Mockito vào `pubspec.yaml` của bạn:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Giả sử bạn có một lớp `UserRepository` lấy dữ liệu người dùng, và bạn muốn kiểm tra `UserService` phụ thuộc vào `UserRepository` mà không tác động đến cơ sở dữ liệu thực:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Tạo một lớp Mock sử dụng Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Các kiểm tra UserService', () {
    test('Lấy người dùng thành công', () {
      // Tạo ra một instance mô phỏng
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Thiết lập hành vi mô phỏng
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // Kiểm tra rằng phương thức mô phỏng được gọi với đối số mong đợi
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Chạy bài kiểm tra này xác nhận rằng `UserService` tương tác đúng cách với `UserRepository`, sử dụng mô phỏng để mô phỏng các tương tác thực một cách có kiểm soát.

---
title:                "Sắp xếp mã lập trình vào hàm số"
date:                  2024-03-08T21:56:15.606908-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tổ chức code thành các hàm trong Dart là về việc định nghĩa các khối code có thể tái sử dụng thực hiện các tác vụ cụ thể, thường nhận đầu vào, xử lý dữ liệu và có thể trả về đầu ra. Lập trình viên làm điều này để tăng cường khả năng đọc code, giảm bớt sự trùng lặp và tạo thuận lợi cho việc bảo trì, cuối cùng dẫn đến việc có được các cơ sở mã dễ quản lý và mô-đun hơn.

## Làm thế nào:
### Hàm Cơ Bản
Trong Dart, bạn định nghĩa một hàm sử dụng từ khóa `void` nếu nó không trả về giá trị, hoặc xác định kiểu giá trị mà nó trả về nếu có. Dưới đây là một hàm đơn giản in ra một thông điệp chào hỏi:

```dart
void greet(String name) {
  print('Hello, $name!');
}

void main() {
  greet('Alice');  // Kết quả: Hello, Alice!
}
```

### Trả về một Giá Trị
Các hàm có thể trả về giá trị. Ví dụ sau nhận hai số nguyên làm đầu vào và trả về tổng của chúng:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // Kết quả: 8
}
```

### Hàm Ẩn Danh
Dart hỗ trợ hàm ẩn danh (còn được gọi là biểu thức lambda hoặc closures), có thể tiện lợi cho các chức năng ngắn, tạo ra tại chỗ. Dưới đây là cách sử dụng một hàm ẩn danh với phương thức `forEach` của danh sách:

```dart
void main() {
  var fruits = ['apple', 'banana', 'cherry'];
  fruits.forEach((item) {
    print(item);
  });
  // Kết quả:
  // apple
  // banana
  // cherry
}
```

### Cú pháp Mũi Tên cho Hàm Một Biểu Thức
Đối với các hàm chỉ chứa một biểu thức duy nhất, Dart cung cấp cú pháp ngắn gọn sử dụng ký hiệu "mũi tên" (`=>`). Điều này đặc biệt hữu ích cho các hàm ngắn hoặc khi truyền hàm làm đối số:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Kết quả: 16
}
```

### Sử dụng Thư Viện Bên Thứ Ba
Đối với các chức năng phức tạp hoặc chuyên biệt hơn, các lập trình viên Dart thường dựa vào các thư viện bên thứ ba. Xem xét thư viện `http` để thực hiện các yêu cầu HTTP. Đầu tiên, thêm `http` vào file pubspec.yaml của bạn dưới dependencies:

```
dependencies:
  http: ^0.13.3
```

Sau đó, bạn có thể sử dụng nó để lấy dữ liệu từ web:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Kết quả mong đợi: Dữ liệu JSON của người dùng. Kết quả thực tế sẽ phụ thuộc vào phản hồi của API.
}
```

Nhớ rằng, khi tổ chức code Dart của bạn thành các hàm, hãy suy nghĩ về khả năng tái sử dụng, rõ ràng, và nguyên tắc trách nhiệm đơn lẻ. Điều này không chỉ làm cho code của bạn sạch hơn mà còn dễ dàng hơn cho người khác (và bạn trong tương lai) để hiểu và bảo trì.

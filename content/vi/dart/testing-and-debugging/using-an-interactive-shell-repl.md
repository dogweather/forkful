---
title:                "Sử dụng giao diện dòng lệnh tương tác (REPL)"
date:                  2024-03-08T21:56:56.515646-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Một shell tương tác (REPL - Vòng lặp Đọc-Đánh giá-In) cho Dart cho phép lập trình viên đánh máy và thực thi mã Dart từng dòng một mà không cần phải biên dịch toàn bộ kịch bản. Công cụ này vô giá đối với việc học cú pháp của Dart, thử nghiệm các đoạn mã, hoặc gỡ lỗi bằng cách cung cấp phản hồi ngay lập tức và tạo điều kiện cho việc kiểm tra lặp.

## Làm thế nào:

Dart không đi kèm với REPL được tích hợp sẵn. Tuy nhiên, bạn có thể đạt được chức năng tương tự REPL bằng cách sử dụng DartPad (trực tuyến) hoặc bằng cách sử dụng các công cụ bên thứ ba như `dart_repl`.

**Sử dụng DartPad:**

DartPad (https://dartpad.dev) là một trình biên soạn Dart trực tuyến cho phép bạn viết và chạy mã Dart trong trình duyệt web của mình. Mặc dù không phải là một REPL dòng lệnh truyền thống, nhưng nó cung cấp một trải nghiệm tương tự cho việc thử nghiệm nhanh chóng.

Chỉ cần truy cập vào trang web, nhập mã Dart của bạn vào khung bên trái và nhấn "Run" để xem kết quả hiển thị bên phải.

Ví dụ:
```dart
void main() {
  print('Xin chào, Dart!');
}
```
Kết quả:
```
Xin chào, Dart!
```

**Sử dụng `dart_repl` (công cụ bên thứ ba):**

Đầu tiên, cài đặt `dart_repl` thông qua pub một cách toàn cầu:

```shell
dart pub global activate dart_repl
```

Sau đó, chạy `dart_repl` từ terminal của bạn:

```shell
dart_repl
```

Bây giờ, bạn có thể bắt đầu nhập các câu lệnh Dart trực tiếp vào shell. Ví dụ:

```dart
>>> print('Xin chào, REPL!');
Xin chào, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Những phương pháp này cung cấp một con đường nhanh chóng để thử nghiệm mã Dart ngay lập tức, giúp giảm đáng kể độ dốc của quá trình học và tăng cường năng suất.

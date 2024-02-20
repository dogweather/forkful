---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:23.370038-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p ch\u01B0\u01A1ng tr\xECnh Swift c\u1EE7a b\u1EA1n nh\u1EADn th\xEAm chi ti\u1EBF\
  t khi ng\u01B0\u1EDDi d\xF9ng ch\u1EA1y n\xF3. \u0110i\u1EC1u n\xE0y quan tr\u1ECD\
  ng v\xEC n\xF3 th\xEAm kh\u1EA3 n\u0103ng t\xF9y ch\u1EC9nh\u2026"
lastmod: 2024-02-19 22:04:56.325454
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p ch\u01B0\u01A1ng tr\xECnh Swift c\u1EE7a b\u1EA1n nh\u1EADn th\xEAm chi ti\u1EBF\
  t khi ng\u01B0\u1EDDi d\xF9ng ch\u1EA1y n\xF3. \u0110i\u1EC1u n\xE0y quan tr\u1ECD\
  ng v\xEC n\xF3 th\xEAm kh\u1EA3 n\u0103ng t\xF9y ch\u1EC9nh\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc các đối số dòng lệnh cho phép chương trình Swift của bạn nhận thêm chi tiết khi người dùng chạy nó. Điều này quan trọng vì nó thêm khả năng tùy chỉnh và kiểm soát mà không cần tương tác của người dùng trong khi chương trình đang chạy.

## Cách thức:

Swift làm cho việc đọc các đối số dòng lệnh cực kỳ đơn giản. Chúng có thể truy cập thông qua cấu trúc `CommandLine`. Dưới đây là điểm chính:

```swift
for argument in CommandLine.arguments {
    print(argument)
}
```

Nếu bạn chèn đoạn này vào một tệp `main.swift` và chạy chương trình của mình với một số văn bản bổ sung, như `swift run YourProgram foo bar`, đầu ra của bạn sẽ như thế này:

```
/path/to/YourProgram
foo
bar
```

Đó là từng đối số được in ra, bao gồm cả đường dẫn tới chương trình của bạn làm phần tử đầu tiên - luôn nhớ điều này!

## Sâu hơn nữa

Theo lịch sử, các đối số dòng lệnh đã là một phần cốt lõi trong lập trình, cho phép mọi người tùy chỉnh hành vi của một chương trình mà không cần thay đổi mã. Đây là di sản của Unix, và hầu hết tất cả các ngôn ngữ đều hỗ trợ tính năng này.

Trong Swift, `CommandLine.arguments` là một mảng các chuỗi, mỗi phần tử là một đoạn nhập của bạn, được chia bởi khoảng trắng. Mảng này được trao bởi hệ điều hành khi chương trình của bạn bắt đầu; Swift chỉ làm cho việc truy cập nó trở nên dễ dàng.

Bên cạnh `CommandLine.arguments`, bạn có thể sâu vào việc phân tích phức tạp hơn với các thư viện như `Swift Argument Parser` cho công việc nặng nhọc hơn. Điều này hữu ích khi bạn cần hơn là chỉ những đầu vào đơn giản - nghĩ đến cờ lệnh, tùy chọn, và sub-commands.

Về mặt thực hiện, những đối số dòng lệnh này được truyền tới bạn thông qua một mảng C ẩn đằng sau - `argc` và `argv` quen thuộc. Swift giữ nó ẩn giấu nhưng vẫn giữ lại cùng một hành vi cơ bản mà bạn sẽ tìm thấy trong C hoặc C++.

## Xem thêm

- Để hiểu rộng hơn về chương trình dòng lệnh trong Swift, hãy xem [Tài liệu Swift.org](https://swift.org/getting-started/#using-the-package-manager).
- Để nâng cao trò chơi phân tích đối số của mình, hãy xem [kho GitHub Swift Argument Parser](https://github.com/apple/swift-argument-parser) để biết thêm các thiết lập phức tạp hơn.
- Nếu bạn tò mò về cách các ngôn ngữ khác xử lý điều này, hãy thử so sánh với `sys.argv` của Python hoặc `process.argv` của Node.

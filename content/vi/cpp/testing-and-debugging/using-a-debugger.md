---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:21.678599-07:00
description: "S\u1EED d\u1EE5ng m\u1ED9t debugger c\xF3 ngh\u0129a l\xE0 kh\u1EDF\
  i \u0111\u1ED9ng m\u1ED9t c\xF4ng c\u1EE5 cho ph\xE9p b\u1EA1n nh\xECn v\xE0o b\xEA\
  n trong ch\u01B0\u01A1ng tr\xECnh \u0111ang ch\u1EA1y c\u1EE7a m\xECnh \u0111\u1EC3\
  \ hi\u1EC3u r\xF5 \u0111i\u1EC1u g\xEC \u0111ang th\u1EF1c s\u1EF1 x\u1EA3y\u2026"
lastmod: 2024-02-19 22:04:56.251344
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\u1ED9t debugger c\xF3 ngh\u0129a l\xE0 kh\u1EDFi \u0111\
  \u1ED9ng m\u1ED9t c\xF4ng c\u1EE5 cho ph\xE9p b\u1EA1n nh\xECn v\xE0o b\xEAn trong\
  \ ch\u01B0\u01A1ng tr\xECnh \u0111ang ch\u1EA1y c\u1EE7a m\xECnh \u0111\u1EC3 hi\u1EC3\
  u r\xF5 \u0111i\u1EC1u g\xEC \u0111ang th\u1EF1c s\u1EF1 x\u1EA3y\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Sử dụng một debugger có nghĩa là khởi động một công cụ cho phép bạn nhìn vào bên trong chương trình đang chạy của mình để hiểu rõ điều gì đang thực sự xảy ra. Lập trình viên làm điều này để tìm và loại bỏ lỗi—những vấn đề phiền toái khiến mã của bạn hoạt động không như mong đợi hoặc bị sập.

## Làm thế nào:
C++ tích hợp với các debugger như GDB hay debugger của Visual Studio. Dưới đây là một ví dụ nhỏ sử dụng GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Oops, chia cho số không!
    std::cout << c << std::endl;
    return 0;
}

// Biên dịch với:
// g++ -g -o my_program my_program.cpp

// Chạy với debugger:
// gdb ./my_program
```

Sau khi bạn bắt đầu GDB, bạn có thể đặt các breakpoints, bước qua mã của mình, kiểm tra các biến và nhiều hơn nữa. Nếu bạn chạy ví dụ trên, bạn sẽ thấy chương trình của mình sập do phép chia cho số không.

## Sâu hơn
Việc gỡ lỗi có nguồn gốc từ những ngày đầu của lập trình, nơi cần phải thực sự loại bỏ các lỗi (côn trùng!) khỏi phần cứng. Từ đó, các công cụ gỡ lỗi đã phát triển thành phần mềm phức tạp và mạnh mẽ, quan trọng cho sự phát triển.

Các lựa chọn khác cho GDB với C++ bao gồm LLDB, cũng như debugger tích hợp IDE như trong Visual Studio, CLion, hoặc Eclipse. Những môi trường hiện đại này cung cấp giao diện đồ họa làm cho việc gỡ lỗi ít đáng sợ hơn.

Chi tiết triển khai về việc sử dụng một debugger thường phụ thuộc vào môi trường phát triển của bạn:

- Debugger dòng lệnh (GDB, LLDB) đòi hỏi sự quen thuộc với các lệnh terminal và thường liên quan đến một đường cong học tập gắt gao.
- Các debugger đồ họa đơn giản hóa quy trình bằng cách cho phép tương tác điểm-và-nhấp để đặt các breakpoints, bước qua mã và quan sát các biến.

Hiểu rõ khả năng của debugger của bạn, như các breakpoints điều kiện, điểm theo dõi, hoặc đánh giá các biểu thức, có thể cải thiện đáng kể hiệu suất của bạn trong việc chẩn đoán các vấn đề.

## Xem thêm
- [Tài liệu GDB](https://www.gnu.org/software/gdb/documentation/)
- [Tài liệu Lệnh LLDB](https://lldb.llvm.org/use/map.html)
- [Hướng dẫn Debugger của Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [Gỡ lỗi với CLion](https://www.jetbrains.com/help/clion/debugging-code.html)

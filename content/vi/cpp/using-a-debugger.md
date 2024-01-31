---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:21.678599-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

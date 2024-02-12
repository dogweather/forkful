---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- vi/cpp/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:42.562079-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
REPL (Read-Eval-Print-Loop - Vòng Đọc-Đánh Giá-In) là một môi trường lập trình tương tác đơn giản. Lập trình viên sử dụng nó cho việc thử nghiệm ngôn ngữ theo thời gian thực, các tác vụ nhanh chóng, hoặc để hiểu mới các khái niệm mà không cần đến việc tạo ra các ứng dụng đầy đủ.

## Làm thế nào:
C++ không đi kèm với một REPL tích hợp sẵn, nhưng các công cụ như Cling cung cấp khả năng đó. Dưới đây là cách sử dụng Cling để tính tổng của hai số:
```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "Tổng là: " << a + b << std::endl;
    return 0;
}

// Kết quả đầu ra:
// Tổng là: 12
```

Khởi động Cling và nhập mã từng dòng một, quan sát kết quả đầu ra sau mỗi lệnh. Đó là phản hồi tức thì, không cần biên dịch.

## Khám phá sâu hơn
REPL phổ biến đối với nhưng ngôn ngữ như Python hay Lisp, và chúng đã tồn tại từ những năm 1960. Đối với C++, một ngôn ngữ được biên dịch, khái niệm này không phù hợp một cách tự nhiên, đó là lý do tại sao các công cụ như Cling tồn tại—chúng giúp giả dịch C++ tức thì. Các phương án khác bao gồm các trình biên dịch trực tuyến hoặc các chương trình thử nghiệm quy mô nhỏ được biên dịch theo cách truyền thống. Cling được xây dựng trên LLVM và Clang, cung cấp một cầu nối cho C++ để được sử dụng theo cách giải dịch.

## Xem thêm
- [Cling](https://root.cern/cling/): Một trình giải dịch C++ tương tác, được xây dựng trên nền tảng của các thư viện LLVM và Clang.
- [Jupyter Notebooks](https://jupyter.org/): Cung cấp một vỏ bọc tương tác trong một môi trường sổ ghi chú, hỗ trợ C++ thông qua kernel xeus-cling.
- [LLVM](https://llvm.org/): Một bộ sưu tập các công nghệ trình biên dịch và bộ công cụ có thể tái sử dụng và modular, mà Cling xây dựng dựa trên đó.

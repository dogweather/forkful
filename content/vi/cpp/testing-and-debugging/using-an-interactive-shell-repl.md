---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:42.562079-07:00
description: "REPL (Read-Eval-Print-Loop - V\xF2ng \u0110\u1ECDc-\u0110\xE1nh Gi\xE1\
  -In) l\xE0 m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1\
  c \u0111\u01A1n gi\u1EA3n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 cho\
  \ vi\u1EC7c th\u1EED nghi\u1EC7m ng\xF4n ng\u1EEF\u2026"
lastmod: '2024-03-13T22:44:37.044690-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print-Loop - V\xF2ng \u0110\u1ECDc-\u0110\xE1nh Gi\xE1-In)\
  \ l\xE0 m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1\
  c \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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

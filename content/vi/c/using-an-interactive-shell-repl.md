---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:22.992988-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Một shell tương tác, hay Vòng lặp Đọc-Đánh giá-In (REPL), là một công cụ cung cấp môi trường lập trình thời gian thực để kiểm thử các đoạn mã ngay lập tức. Các lập trình viên sử dụng nó để nhận phản hồi nhanh chóng trong quá trình phát triển, học tập và gỡ lỗi.

## Làm thế nào:
C không đi kèm với REPL tích hợp sẵn, nhưng bạn có thể sử dụng các công cụ của bên thứ ba. Dưới đây là một cái nhìn qua việc sử dụng Cling, một trình thông dịch C++ có thể xử lý cả mã C:

```C
#include <stdio.h>

int main() {
    printf("Xin chào, thế giới REPL!\n");
    return 0;
}
```

Kết quả trong Cling REPL:
```
[cling]$ .x yourscript.c
Xin chào, thế giới REPL!
```

Cling thực hiện kịch bản và in kết quả ngay lập tức.

## Sâu hơn
REPLs phổ biến trong các ngôn ngữ động như Python hay Ruby, nhưng đối với các ngôn ngữ biên dịch như C, chúng ít phổ biến hơn. Lịch sử, vòng lặp biên dịch-chạy-gỡ lỗi không hỗ trợ khám phá tương tác. Các công cụ như Cling và trình biên dịch C trực tuyến cung cấp trải nghiệm giống như REPL bằng cách đóng gói mã C trong một môi trường C++.

Các lựa chọn thay thế cho Cling bao gồm các trình thông dịch C như CINT và Ch. Những công cụ này cho phép lặp lại nhanh chóng nhưng có thể không phù hợp với tất cả các tình huống phát triển do hạn chế về hiệu suất và hỗ trợ cho các tính năng phức tạp.

Việc triển khai một REPL trong một ngôn ngữ biên dịch bao gồm việc biên dịch và thực thi các đoạn mã tức thì, điều này không đơn giản và có thể có những hạn chế so với khả năng của ngôn ngữ đầy đủ.

## Xem thêm
- Cling: https://github.com/root-project/cling
- Trình biên dịch và REPL C trực tuyến: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Trình thông dịch Ch: http://www.softintegration.com/products/chstandard/

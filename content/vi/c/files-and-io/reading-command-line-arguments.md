---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:43.903618-07:00
description: "Trong l\u1EADp tr\xECnh C, vi\u1EC7c \u0111\u1ECDc c\xE1c \u0111\u1ED1\
  i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p c\xE1c ch\u01B0\u01A1ng tr\xECnh nh\u1EAD\
  n \u0111\u1EA7u v\xE0o ngay t\u1EEB terminal, l\xE0m t\u0103ng t\xEDnh linh ho\u1EA1\
  t v\xE0 kh\u1EA3 n\u0103ng s\u1EED d\u1EE5ng. C\xE1c\u2026"
lastmod: '2024-02-25T18:49:35.636503-07:00'
model: gpt-4-0125-preview
summary: "Trong l\u1EADp tr\xECnh C, vi\u1EC7c \u0111\u1ECDc c\xE1c \u0111\u1ED1i\
  \ s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p c\xE1c ch\u01B0\u01A1ng tr\xECnh nh\u1EAD\
  n \u0111\u1EA7u v\xE0o ngay t\u1EEB terminal, l\xE0m t\u0103ng t\xEDnh linh ho\u1EA1\
  t v\xE0 kh\u1EA3 n\u0103ng s\u1EED d\u1EE5ng. C\xE1c\u2026"
title: "\u0110\u1ECDc c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trong lập trình C, việc đọc các đối số dòng lệnh cho phép các chương trình nhận đầu vào ngay từ terminal, làm tăng tính linh hoạt và khả năng sử dụng. Các lập trình viên tận dụng điều này để cấu hình hành vi của script mà không cần chỉnh sửa mã, khiến cho ứng dụng trở nên linh hoạt và hiệu quả.

## Làm thế nào:

Trong C, hàm `main` có thể được thiết kế để chấp nhận các đối số dòng lệnh sử dụng các tham số `int argc` và `char *argv[]`. Tại đây, `argc` đại diện cho số lượng đối số được truyền và `argv` là một mảng các con trỏ ký tự liệt kê tất cả các đối số. Dưới đây là một ví dụ nhanh để minh họa:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Tên chương trình: %s\n", argv[0]);
    printf("Số lượng Đối số: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Đối số %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Sử dụng đoạn mã trên, nếu chương trình được thực thi như `./programName -a example`, đầu ra sẽ là:

```
Tên chương trình: ./programName
Số lượng Đối số: 2
Đối số 1: -a
Đối số 2: example
```

Điều này minh họa việc đối số dòng lệnh có thể được phân tích và sử dụng trong một chương trình C.

## Sâu hơn

Quy ước truyền đối số cho chương trình có từ những ngày đầu tiên của Unix. Trong cách tiếp cận truyền thống này, `argc` và `argv` cung cấp một giao diện đơn giản nhưng mạnh mẽ cho tương tác dòng lệnh, thể hiện triết lý của Unix về các công cụ nhỏ gọn, mô-đun có thể làm việc cùng nhau. Mặc dù các ngôn ngữ hiện đại thường giới thiệu các thư viện hoặc khung làm việc phức tạp hơn cho việc phân tích đối số dòng lệnh, nhưng sự trực tiếp của phương pháp C đem lại sự trong suốt và kiểm soát không thể so sánh.

Trong các phát triển gần đây, các thư viện như `getopt` trong các hệ thống POSIX đã phát triển để hỗ trợ nhu cầu phân tích phức tạp hơn, như xử lý tên tùy chọn dài hoặc giá trị mặc định cho các đối số còn thiếu. Tuy nhiên, cơ chế cơ bản của `argc` và `argv` vẫn là cần thiết để hiểu cách các chương trình tương tác với môi trường thời gian chạy của mình trong C.

Những người chỉ trích có thể cho rằng việc xử lý trực tiếp `argc` và `argv` có thể gây ra lỗi, thúc giục sử dụng các trừu tượng hóa cấp cao hơn. Tuy nhiên, đối với những người tìm hiểu sâu vào chi tiết của C và đánh giá cao sự tinh tế trong hoạt động mức thấp của nó, việc thành thạo phân tích đối số dòng lệnh là một bước ngoặt. Sự kết hợp giữa phương pháp lịch sử và tiện ích thực tế chứa đựng phần lớn sức hấp dẫn lâu dài của C trong lập trình hệ thống và phát triển phần mềm.

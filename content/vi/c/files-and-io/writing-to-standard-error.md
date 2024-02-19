---
aliases:
- /vi/c/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:34.659571-07:00
description: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n trong C li\xEAn quan \u0111\
  \u1EBFn vi\u1EC7c \u0111i\u1EC1u h\u01B0\u1EDBng c\xE1c th\xF4ng b\xE1o l\u1ED7\
  i v\xE0 th\xF4ng tin ch\u1EA9n \u0111o\xE1n sang m\u1ED9t lu\u1ED3ng ri\xEAng bi\u1EC7\
  t kh\u1ECFi \u0111\u1EA7u ra ch\xEDnh c\u1EE7a\u2026"
lastmod: 2024-02-18 23:08:51.253389
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n trong C li\xEAn quan \u0111\u1EBF\
  n vi\u1EC7c \u0111i\u1EC1u h\u01B0\u1EDBng c\xE1c th\xF4ng b\xE1o l\u1ED7i v\xE0\
  \ th\xF4ng tin ch\u1EA9n \u0111o\xE1n sang m\u1ED9t lu\u1ED3ng ri\xEAng bi\u1EC7\
  t kh\u1ECFi \u0111\u1EA7u ra ch\xEDnh c\u1EE7a\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc ghi vào lỗi chuẩn trong C liên quan đến việc điều hướng các thông báo lỗi và thông tin chẩn đoán sang một luồng riêng biệt khỏi đầu ra chính của chương trình. Các lập trình viên làm điều này để phân chia thông báo lỗi khỏi đầu ra chuẩn, giúp cả hai dễ đọc và xử lý riêng biệt hơn, đặc biệt là khi gỡ lỗi hoặc ghi lại quá trình thực thi của các chương trình.

## Cách thực hiện:

Trong C, luồng `stderr` được sử dụng để viết thông báo lỗi. Không giống như viết vào đầu ra chuẩn với `printf`, viết vào `stderr` có thể được thực hiện bằng cách sử dụng `fprintf` hoặc `fputs`. Đây là cách bạn có thể làm:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Đây là một thông báo lỗi.\n");

    fputs("Đây là một thông báo lỗi khác.\n", stderr);
    
    return 0;
}
```

Đầu ra mẫu (tới stderr):
```
Đây là một thông báo lỗi.
Đây là một thông báo lỗi khác.
```

Quan trọng là nên lưu ý rằng mặc dù đầu ra trông tương tự như `stdout` trên bảng điều khiển, khi sử dụng chuyển hướng trong terminal, sự khác biệt trở nên rõ ràng:

```sh
$ ./chương_trình_của_bạn > output.txt
```

Lệnh này chỉ chuyển hướng đầu ra chuẩn tới `output.txt`, trong khi các thông báo lỗi vẫn sẽ xuất hiện trên màn hình.

## Tìm hiểu sâu

Sự khác biệt giữa `stdout` và `stderr` trong các hệ thống dựa trên Unix có từ những ngày đầu tiên của C và Unix. Sự phân chia này cho phép xử lý lỗi mạnh mẽ hơn và ghi nhật ký, vì nó cho phép lập trình viên chuyển hướng các thông báo lỗi một cách độc lập với đầu ra chương trình chuẩn. Mặc dù `stderr` không được bộ đệm theo mặc định để đảm bảo đầu ra tức thì của các thông báo lỗi, điều này giúp trong việc gỡ lỗi sự cố và các vấn đề quan trọng khác, trong khi đó `stdout` thường được bộ đệm, có nghĩa là đầu ra của nó có thể bị trì hoãn cho đến khi bộ đệm được làm rỗng (ví dụ: khi chương trình kết thúc hoặc làm rỗng bằng tay).

Trong các ứng dụng hiện đại, việc viết vào `stderr` vẫn còn liên quan, đặc biệt là đối với các công cụ dòng lệnh và ứng dụng máy chủ nơi việc phân biệt giữa các thông điệp nhật ký thông thường và lỗi là rất quan trọng. Tuy nhiên, đối với việc xử lý lỗi phức tạp hơn, đặc biệt là trong các ứng dụng giao diện người dùng đồ họa hoặc nơi cần có cơ chế ghi nhật ký tinh vi hơn, lập trình viên có thể sử dụng các thư viện ghi nhật ký chuyên dụng cung cấp nhiều kiểm soát hơn về định dạng thông điệp, điểm đến (ví dụ: tệp tin, mạng), và cấp độ nghiêm trọng (thông tin, cảnh báo, lỗi, v.v.).

Dù `stderr` cung cấp một cơ chế cơ bản cho báo cáo lỗi trong C, sự phát triển của các thực hành lập trình và sự có sẵn của các khuôn khổ ghi nhật ký tiên tiến có nghĩa là nó thường chỉ là điểm xuất phát cho các chiến lược xử lý lỗi hiện đại.

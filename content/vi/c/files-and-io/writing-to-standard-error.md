---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:34.659571-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong C, lu\u1ED3ng `stderr` \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 vi\u1EBFt th\xF4ng b\xE1o l\u1ED7i. Kh\xF4\
  ng gi\u1ED1ng nh\u01B0 vi\u1EBFt v\xE0o \u0111\u1EA7u ra chu\u1EA9n v\u1EDBi `printf`,\
  \ vi\u1EBFt v\xE0o `stderr` c\xF3 th\u1EC3\u2026"
lastmod: '2024-03-13T22:44:37.290193-06:00'
model: gpt-4-0125-preview
summary: "Trong C, lu\u1ED3ng `stderr` \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\
  \u1EC3 vi\u1EBFt th\xF4ng b\xE1o l\u1ED7i."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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

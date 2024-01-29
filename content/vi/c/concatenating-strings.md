---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:57:05.951321-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối để tạo thành một chuỗi mới. Các lập trình viên thực hiện điều này để kết hợp văn bản theo những cách động, như xây dựng tin nhắn hoặc tạo đường dẫn tệp.

## Cách thực hiện:

Trong C, bạn sử dụng hàm `strcat` từ `string.h` để nối chuỗi. Nhưng hãy cẩn thận, bạn cần một mảng đích đủ lớn để chứa kết quả kết hợp.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Xin chào, ";
    char source[] = "thế giới!";

    // Nối `source` vào `destination`
    strcat(destination, source);

    // Xuất ra chuỗi đã nối
    printf("%s\n", destination); // "Xin chào, thế giới!"

    return 0;
}
```

Hãy chắc chắn rằng mảng đích của bạn không tràn. Đó là trách nhiệm của bạn, vì C không làm điều đó cho bạn.

## Tìm hiểu sâu hơn

Nối chuỗi đã là một thao tác văn bản cơ bản kể từ những ngày đầu của máy tính. Trong C, các hàm như `strcat` và `strncat` (giới hạn số ký tự được nối) thực hiện công việc nặng nhọc. C không quản lý bộ nhớ cho bạn, vì vậy nhớ cấp phát đủ không gian trước khi bạn nối.

Có phương án thay thế không? Ồ, chắc chắn rồi. Nếu bạn lo lắng về tràn bộ đệm, bạn có thể sử dụng `snprintf` thay thế. Nó an toàn hơn vì cho phép bạn chỉ định kích thước tối đa của bộ đệm đầu ra:

```C
char buffer[50];
snprintf(buffer, 50, "%s%s", "Xin chào, ", "thế giới!");
```

Về mặt chi tiết, `strcat` hoạt động bằng cách tìm điểm cuối của chuỗi đầu tiên và sao chép chuỗi thứ hai vào đó từng ký tự một. Đơn giản, nhưng việc quản lý bộ nhớ thủ công khiến nó dễ dàng gặp phải lỗi như tràn bộ đệm.

## Xem thêm

- Tài liệu Thư viện Chuẩn C cho `strcat`: https://en.cppreference.com/w/c/string/byte/strcat
- Lập trình an toàn với C: https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard
- Tìm hiểu thêm về tràn bộ đệm: https://owasp.org/www-community/attacks/Buffer_overflow

---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:04.409992-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xóa bỏ dấu ngoặc khỏi một chuỗi nghĩa là loại bỏ bất kỳ dấu ngoặc kép ("") hoặc dấu ngoặc đơn ('') nào là một phần của nội dung chuỗi. Các lập trình viên thực hiện điều này để làm sạch dữ liệu đầu vào, chuẩn bị dữ liệu cho quá trình xử lý tiếp theo, hoặc tránh các lỗi cú pháp khi xử lý đường dẫn tệp và lệnh trong các ngôn ngữ sử dụng dấu ngoặc để phân biệt chuỗi.

## Cách thực hiện:

Dưới đây là một hàm C sẽ loại bỏ những dấu ngoặc phiền toái khỏi chuỗi của bạn:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Ban đầu: %s\n", str);
    remove_quotes(str);
    printf("Đã làm sạch: %s\n", str);
    return 0;
}
```

Kết quả mẫu:

```
Ban đầu: He said, "Hello, 'world'!"
Đã làm sạch: He said, Hello, world!
```

## Sâu hơn

Việc xóa bỏ dấu ngoặc khỏi một chuỗi đã là một nhiệm vụ từ thuở sơ khai của lập trình, nơi vệ sinh dữ liệu là và vẫn là chìa khóa để tránh lỗi (như các cuộc tấn công SQL injection) hoặc đảm bảo một chuỗi có thể an toàn được truyền đến các hệ thống có thể nhầm lẫn dấu ngoặc là một ký tự điều khiển.

Theo lịch sử, các ngôn ngữ khác nhau xử lý nhiệm vụ này theo cách khác nhau—một số có các hàm tích hợp sẵn (như `strip` trong Python), trong khi những ngôn ngữ khác, như C, đòi hỏi phải thực hiện thủ công do tập trung vào việc cung cấp quyền kiểm soát mức thấp hơn cho nhà phát triển.

Các phương án thay thế bao gồm sử dụng các hàm thư viện như `strpbrk` để tìm dấu ngoặc hoặc sử dụng biểu thức chính quy (với các thư viện như PCRE) cho các mô hình phức tạp hơn, mặc dù điều này có thể là quá mức cho việc đơn giản là xóa bỏ dấu ngoặc.

Cài đặt trên đơn giản là quét qua từng ký tự trong chuỗi, chỉ sao chép các ký tự không phải dấu ngoặc vào vị trí con trỏ ghi. Điều này hiệu quả vì nó được thực hiện ngay tại chỗ mà không cần bộ nhớ phụ trợ cho chuỗi kết quả.

## Xem thêm

- [Các Hàm Thư Viện Chuẩn C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Hiểu về Con trỏ trong C](https://www.learn-c.org/en/Pointers)

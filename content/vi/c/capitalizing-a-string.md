---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:36.120788-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Việc in hoa một chuỗi có nghĩa là chuyển tất cả các chữ cái viết thường thành chữ cái viết hoa. Các lập trình viên thường in hoa các chuỗi để đảm bảo tính nhất quán, định dạng hiển thị, hoặc như một phần của quá trình chuẩn hóa dữ liệu.

## Làm Thế Nào:
Ngôn ngữ C không có một hàm tích hợp sẵn để in hoa chuỗi. Bạn sẽ thường xuyên lặp qua từng ký tự, in hoa từng cái một:

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "hello world!";
    capitalizeString(myString);
    printf("%s\n", myString);  // Đầu ra: HELLO WORLD!
    return 0;
}
```

## Sâu Hơn
Trong những ngày đầu của việc tính toán, các thao tác trên chuỗi là cơ bản và thủ công. C, được phát triển vào đầu những năm 1970, phản ánh điều này với các hàm thao tác chuỗi đơn giản trong thư viện chuẩn của mình. Hàm `toupper` được thiết kế để chuyển một ký tự đơn thành chữ cái in hoa. Nó là một phần của `<ctype.h>`, một tiêu đề chứa các hàm để kiểm tra và ánh xạ các ký tự.

Có các phương án thay thế cho việc lặp qua một chuỗi để in hoa nó. Thư viện như `libCStringUtils` cung cấp các thao tác chuỗi phức tạp hơn, bao gồm cả việc in hoa. Một số nhà phát triển cũng tự viết các hàm của họ với các tính năng như nhạy cảm với địa phương.

Nội bộ, các ký tự ASCII có tương đương số học, chênh lệch 32 giữa chữ viết thường và chữ viết hoa. Hàm `toupper` sử dụng sự khác biệt này để chuyển đổi ký tự. Tuy nhiên, việc dựa vào giá trị ASCII trực tiếp không được khuyến khích do vấn đề đọc và địa phương hóa.

## Xem Thêm
- Tài liệu Thư viện Chuẩn C: https://en.cppreference.com/w/c/header
- Bảng và Mô tả ASCII: http://www.asciitable.com/
- Hướng dẫn GNU Libc: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html

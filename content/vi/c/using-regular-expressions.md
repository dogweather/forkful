---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:20.499379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Biểu thức chính quy (regex) dùng để tìm kiếm, khớp và thao tác với chuỗi. Các lập trình viên sử dụng chúng cho việc xác thực văn bản, tìm kiếm và biến đổi, giúp tăng tốc độ xử lý văn bản.

## Làm thế nào:
C không hỗ trợ regex một cách tích hợp sẵn, nhưng bạn có thể sử dụng các thư viện như `regex.h`. Dưới đây là một ví dụ đơn giản về việc khớp mẫu.

```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int result;
    char *pattern = "^hello";
    char *text = "hello world";

    // Biên dịch regex
    result = regcomp(&regex, pattern, REG_EXTENDED);

    if (result) {
        printf("Biên dịch regex thất bại.\n");
        return 1;
    }

    // Thực thi regex
    result = regexec(&regex, text, 0, NULL, 0);
    
    // Kiểm tra khớp
    if (!result) {
        printf("Tìm thấy khớp.\n");
    } else if (result == REG_NOMATCH) {
        printf("Không tìm thấy khớp.\n");
    } else {
        printf("Thực thi regex thất bại.\n");
    }

    // Giải phóng regex
    regfree(&regex);

    return 0;
}
```
Kết quả Mẫu:
```
Tìm thấy khớp.
```

## Sâu hơn
Biểu thức chính quy đã được sử dụng từ những năm 1950, phổ biến với `ed` và `grep` của Unix. Các lựa chọn thay thế trong C bao gồm các thư viện chức năng chuỗi và bộ phân tích cú pháp tùy chỉnh, nhưng regex linh hoạt hơn. Phía sau, `regex.h` thực thi chức năng regex, thường qua các động cơ NFA (Non-deterministic Finite Automaton) hoặc DFA (Deterministic Finite Automaton).

## Xem Thêm
- Tiêu chuẩn POSIX: https://pubs.opengroup.org/onlinepubs/9699919799/
- Hướng dẫn Biểu thức chính quy (regex): https://www.regular-expressions.info/
- POSIX regex trong C: http://man7.org/linux/man-pages/man3/regcomp.3.html

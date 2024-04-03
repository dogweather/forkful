---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:33.310842-07:00
description: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong C bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t n\u1ED9i dung v\u0103n b\u1EA3\
  n m\xE0 kh\xF4ng c\u1EA7n d\u1EA5u ngo\u1EB7c \u0111\u01A1n (' ') ho\u1EB7c d\u1EA5\
  u ngo\u1EB7c k\xE9p (\" \"). Qu\xE1\u2026"
lastmod: '2024-03-13T22:44:37.251343-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong C bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t n\u1ED9i dung v\u0103n b\u1EA3\
  n m\xE0 kh\xF4ng c\u1EA7n d\u1EA5u ngo\u1EB7c \u0111\u01A1n (' ') ho\u1EB7c d\u1EA5\
  u ngo\u1EB7c k\xE9p (\" \")."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cái gì và Tại sao?

Việc loại bỏ dấu ngoặc khỏi một chuỗi trong C bao gồm việc trích xuất nội dung văn bản mà không cần dấu ngoặc đơn (' ') hoặc dấu ngoặc kép (" "). Quá trình này rất quan trọng để làm sạch dữ liệu đầu vào, phân tích nội dung tệp, hoặc chuẩn bị chuỗi cho quá trình xử lý tiếp theo nơi mà dấu ngoặc không cần thiết hoặc có thể dẫn đến lỗi trong xử lý dữ liệu.

## Làm thế nào:

Để loại bỏ dấu ngoặc khỏi một chuỗi trong C, chúng ta duyệt qua chuỗi, sao chép các ký tự không phải là dấu ngoặc vào một chuỗi mới. Quá trình này có thể được tùy chỉnh để loại bỏ chỉ dấu ngoặc ở đầu và cuối hoặc tất cả dấu ngoặc có trong chuỗi. Dưới đây là một ví dụ minh họa thể hiện cả hai cách tiếp cận:

```c
#include <stdio.h>
#include <string.h>

// Hàm để loại bỏ tất cả dấu ngoặc khỏi một chuỗi
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // Kết thúc chuỗi đích bằng dấu NULL
}

// Hàm để chỉ loại bỏ dấu ngoặc ở đầu và cuối chuỗi
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // Kết thúc chuỗi đích bằng dấu NULL
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("All Quotes Removed: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Dấu Ngoặc ở Biên Đã Được Loại Bỏ: %s\n", noQuotes2);
    
    return 0;
}
```
Kết quả Mẫu:
```
All Quotes Removed: Hello, World!
Dấu Ngoặc ở Biên Đã Được Loại Bỏ: Programming in C
```

Những ví dụ này cho thấy làm thế nào để xử lý việc loại bỏ tất cả dấu ngoặc có trong chuỗi và việc loại bỏ cụ thể chỉ dấu ngoặc ở đầu và cuối.

## Sâu hơn

Khái niệm về việc loại bỏ dấu ngoặc khỏi chuỗi không có chiều sâu lịch sử đáng kể trong C, ngoài mối liên kết của nó với nhu cầu xử lý văn bản từ sớm. Phương pháp tiếp cận trực tiếp được minh họa ở đây linh hoạt nhưng không hiệu quả với chuỗi rất lớn hoặc yêu cầu hiệu suất cao, nơi mà sự chỉnh sửa tại chỗ hoặc các thuật toán tiên tiến hơn có thể được ưa chuộng.

Những phương án thay thế, như sử dụng `strpbrk` để tìm dấu ngoặc và di chuyển phần của chuỗi không có dấu ngoặc, có thể hiệu quả hơn nhưng đòi hỏi hiểu biết sâu hơn về con trỏ và quản lý bộ nhớ trong C. Hơn nữa, sự xuất hiện của các thư viện biểu thức chính quy đã cung cấp một bộ công cụ mạnh mẽ cho việc thao tác chuỗi, bao gồm cả việc loại bỏ dấu ngoặc. Tuy nhiên, những thư viện này, mặc dù mạnh mẽ, thêm vào độ phức tạp và tải quá mức có thể không cần thiết cho các tác vụ đơn giản hơn. Do đó, phương pháp tiếp cận trực tiếp như được trình bày, vẫn là một kỹ năng quý giá đối với các lập trình viên C, kết hợp sự đơn giản với hiệu quả cho nhiều trường hợp sử dụng phổ biến.

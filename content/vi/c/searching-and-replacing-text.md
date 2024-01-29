---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:37.079571-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc tìm và thay thế văn bản trong lập trình bao gồm việc tìm những chuỗi cụ thể và thay thế chúng bằng thứ khác - hãy nghĩ đến nó như là tính năng "tìm và thay thế" trong trình xử lý văn bản của bạn, nhưng dành cho mã. Các lập trình viên sử dụng điều này để tái cấu trúc mã, thao tác dữ liệu, và tự động hóa những chỉnh sửa mà sẽ rất mất thời gian nếu làm bằng tay.

## Cách thực hiện:

Hãy bắt tay vào việc với mã. Chúng ta sử dụng `strstr()` để tìm kiếm và `strcpy()` để thay thế. Dưới đây là một chương trình C đơn giản:

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void searchAndReplace(char *text, const char *search, const char *replace) {
    char *pos, temp[1024];
    int index = 0;
    int searchLen = strlen(search);

    temp[0] = '\0'; // Đảm bảo temp trống

    // Lặp qua văn bản để tìm tất cả các sự xuất hiện của chuỗi tìm kiếm
    while ((pos = strstr(text, search)) != NULL) {
        // Sao chép văn bản dẫn đến chuỗi tìm kiếm
        strncpy(temp + index, text, pos - text);
        index += pos - text;
        
        // Thêm văn bản thay thế
        strcpy(temp + index, replace);
        index += strlen(replace);
        
        // Di chuyển qua chuỗi tìm kiếm trong văn bản
        text = pos + searchLen;
    }
    
    // Thêm bất kỳ văn bản còn lại
    strcpy(temp + index, text);

    // Xuất kết quả
    printf("Văn bản đã thay thế: %s\n", temp);
}

int main() {
    char text[] = "The rain in Spain falls mainly in the plain.";
    char search[] = "ain";
    char replace[] = "ane";

    searchAndReplace(text, search, replace);

    return 0;
}
```
Kết quả mẫu:
```
Văn bản đã thay thế: The rane in Spane falls manely in the plane.
```

## Sâu hơn nữa

Lịch sử, việc xử lý văn bản là một khái niệm cũ, có nguồn gốc từ những công cụ như `sed` trong Unix. C không có tính năng "tìm và thay thế" tích hợp sẵn; do đó, chúng ta kết hợp các hàm xử lý chuỗi.

Các phương án thay thế cho cách tiếp cận của chúng ta bao gồm biểu thức chính quy (regex) - mạnh mẽ nhưng phức tạp - và thư viện của bên thứ ba có thể đem lại nhiều linh hoạt hơn.

Việc hiểu rõ con trỏ, quản lý bộ nhớ cấp phát, và quản lý bộ đệm là rất quan trọng; nếu không, bạn sẽ gặp rủi ro như tràn bộ đệm. Một triển khai kỹ lưỡng sẽ kiểm tra những lỗi này và được điều chỉnh để tối ưu hiệu năng với các văn bản lớn hoặc các thao tác thường xuyên.

## Xem thêm

Để biết thêm ngữ cảnh và các trường hợp sử dụng nâng cao, hãy tham khảo:

- Tài liệu về thư viện chuẩn C về xử lý chuỗi: http://www.cplusplus.com/reference/cstring/
- GNU `sed` cho chỉnh sửa luồng: https://www.gnu.org/software/sed/
- Hướng dẫn về biểu thức chính quy cho việc so khớp mẫu: https://www.regular-expressions.info/tutorial.html
- Giải thích về con trỏ trong C: http://cslibrary.stanford.edu/102/

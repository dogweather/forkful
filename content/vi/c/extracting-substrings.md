---
title:                "Trích xuất chuỗi con"
date:                  2024-01-28T21:59:43.991313-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc trích xuất các chuỗi con có nghĩa là lấy một phần cụ thể của chuỗi - như một lát bánh. Lập trình viên làm điều này để cô lập, xử lý, hoặc thao tác chỉ những bit dữ liệu liên quan trong một văn bản lớn hơn.

## Cách thực hiện:
Hãy thử trích xuất một số chuỗi con từ một chuỗi sử dụng C.

```C
#include <stdio.h>
#include <string.h>

void extract_substring(const char *source, int start, int length, char *dest) {
    strncpy(dest, source + start, length);
    dest[length] = '\0'; // Đừng quên kết thúc bằng ký tự null!
}

int main() {
    const char *full_text = "Extracting substrings is neat.";
    char snippet[20];

    extract_substring(full_text, 0, 10, snippet);
    printf("Đoạn: %s\n", snippet);

    extract_substring(full_text, 12, 10, snippet);
    printf("Một đoạn khác: %s\n", snippet);

    return 0;
}
```

Kết quả mẫu:

```
Đoạn: Extracting
Một đoạn khác: substrings
```

## Sâu hơn nữa
Việc trích xuất các chuỗi con không phải là điều gì mới mẻ. Trong lĩnh vực lập trình C, đây đã là một công việc phổ biến kể từ khi ngôn ngữ này ra đời vào những năm 1970.

Bạn có các cách khác nhau để lấy những chuỗi con đó. Một số người sử dụng `strncpy()`, như ví dụ trên của chúng tôi. Người khác có thể thích `sscanf()` hoặc thậm chí vòng lặp qua chuỗi một cách thủ công. Mỗi phương pháp có những điểm tinh tế của riêng nó. Với `strncpy()`, hãy cảnh giác - nếu độ dài bạn chỉ định vượt quá phần cuối của chuỗi, nó sẽ không tự động thêm ký tự null cho bạn.

Ở bên dưới, một chuỗi chỉ đơn giản là một mảng các ký tự trong C. Khi cắt lát, bạn đang xử lý với các con trỏ đến các địa chỉ cụ thể trong bộ nhớ. Hãy chú ý đến giới hạn và luôn kết thúc các đoạn trích của bạn bằng ký tự null.

## Xem thêm
- `strncpy()` manual: https://www.man7.org/linux/man-pages/man3/strncpy.3.html
- Xử lý chuỗi C: https://en.cppreference.com/w/c/string
- Con trỏ và mảng: https://www.tutorialspoint.com/cprogramming/c_pointers.htm

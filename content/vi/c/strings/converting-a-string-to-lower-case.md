---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/c/converting-a-string-to-lower-case/
date:                  2024-02-03T17:54:51.886201-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc chuyển đổi một chuỗi thành chữ thường trong C bao gồm việc biến đổi tất cả các chữ cái in hoa trong một chuỗi đã cho thành các tương đương chữ thường của chúng. Các lập trình viên thường thực hiện thao tác này để chuẩn hóa đầu vào văn bản cho việc so sánh, tìm kiếm hoặc đơn giản chỉ để đồng nhất về mặt thẩm mỹ trong đầu ra.

## Làm thế nào:

C không có hàm tích hợp sẵn cho việc chuyển đổi chuỗi thành chữ thường trực tiếp, không giống như một số ngôn ngữ cấp cao. Tuy nhiên, quá trình này có thể dễ dàng thực hiện sử dụng các hàm của thư viện chuẩn C. Dưới đây là hướng dẫn từng bước và một ví dụ minh họa cách chuyển đổi một chuỗi thành chữ thường.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original: %s\n", text);

    toLowerCase(text);
    printf("Lowercase: %s\n", text);

    return 0;
}
```

**Kết quả Mẫu:**

```
Original: Hello, World!
Lowercase: hello, world!
```

Trong ví dụ này, hàm `toLowerCase` lặp qua từng ký tự của chuỗi đầu vào, chuyển đổi nó thành tương đương chữ thường sử dụng hàm `tolower` từ `ctype.h`. Sự thay đổi được thực hiện tại chỗ, thay đổi chuỗi gốc.

## Sâu hơn nữa

Hàm `tolower` được sử dụng trong ví dụ trên là một phần của thư viện chuẩn C, cụ thể là trong tệp tiêu đề `ctype.h`. Nó hoạt động dựa trên ngữ cảnh hiện tại, nhưng cho ngữ cảnh "C" chuẩn, nó xử lý bộ ký tự ASCII nơi 'A' đến 'Z' được chuyển đổi thành 'a' đến 'z'.

Trong lịch sử, việc xử lý mã hóa ký tự và chuyển đổi chữ hoa chữ thường trong C đã được liên kết chặt chẽ với bộ ký tự ASCII, giới hạn khả năng sử dụng của nó trong các ứng dụng quốc tế hoặc địa phương hóa nơi các ký tự ngoài bộ ký tự ASCII là phổ biến. Các ngôn ngữ lập trình hiện đại có thể cung cấp các phương thức chuỗi tích hợp sẵn để thực hiện chuyển đổi chữ hoa chữ thường xem xét ngữ cảnh và ký tự Unicode, điều mà C không có sẵn một cách nguyên thủy.

Trong các tình huống đòi hỏi việc thao tác văn bản rộng rãi, đặc biệt là với các ký tự không phải ASCII, các lập trình viên có thể cân nhắc sử dụng các thư viện cung cấp hỗ trợ quốc tế hóa tốt hơn, chẳng hạn như ICU (Bộ phận Quốc tế cho Unicode). Tuy nhiên, đối với hầu hết các ứng dụng xử lý văn bản ASCII, cách tiếp cận được minh họa là hiệu quả và dễ dàng. Nó nêu bật xu hướng của C trong việc trao quyền kiểm soát việc thao tác dữ liệu cho lập trình viên, mặc dù cần phải làm việc nhiều hơn so với ngôn ngữ cấp cao.

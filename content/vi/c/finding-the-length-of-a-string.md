---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:23.576239-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Tìm độ dài của một chuỗi nghĩa là đếm số lượng ký tự mà nó chứa trước ký tự kết thúc null `\0`. Điều này rất quan trọng cho việc thao tác với chuỗi - như khi chúng ta cần lặp qua một chuỗi hoặc phân bổ chính xác không gian bộ nhớ.

## Cách thực hiện:

Lựa chọn của bạn trong C để đo độ dài chuỗi là hàm `strlen` từ `<string.h>`. Dưới đây là cách nó hoạt động:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);  // Sử dụng size_t cho độ dài của chuỗi
    printf("Độ dài của '%s' là %zu.\n", myString, length);
    return 0;
}
```

Kết quả mong đợi:
```
Độ dài của 'Hello, World!' là 13.
```

Nhớ là, `strlen` không tính ký tự kết thúc null.

## Sâu hơn

Trong lịch sử, chuỗi trong C kết thúc với ký tự kết thúc null `\0` - đây là cách các hàm biết nơi mà một chuỗi kết thúc. Điều thú vị: `strlen` bản thân nó là một vòng lặp đơn giản chạy từ đầu chuỗi đến ký tự kết thúc null.

Nếu như `strlen` không phải là sự lựa chọn của bạn? Đối với hệ thống nhúng hoặc mã quan trọng về hiệu suất, bạn có thể viết một hàm độ dài tùy chỉnh để tránh gánh nặng thư viện hoặc xử lý các định dạng chuỗi không tiêu chuẩn. Chỉ cần thận trọng; nó là một lễ hội lỗi nếu làm sai.

Phía sau cánh gà, `strlen` có thể thay đổi từ đơn giản đến phức tạp. Triển khai ngây thơ có thể chỉ là vài dòng mã trong một vòng lặp, trong khi các phiên bản tối ưu hóa có thể sử dụng các kỹ thuật như loop unrolling hoặc xử lý song song để tăng tốc đối với chuỗi lớn.

## Xem Thêm

Đối với những ai muốn tìm hiểu thêm, hãy khám phá những nguồn sau:

- Tài liệu tham khảo Thư viện Chuẩn C cho `strlen`: [https://www.cplusplus.com/reference/cstring/strlen/](https://www.cplusplus.com/reference/cstring/strlen/)
- Đào sâu hiểu biết về cách chuỗi hoạt động trong C: [https://www.cs.swarthmore.edu/~newhall/unixhelp/C_strings.html](https://www.cs.swarthmore.edu/~newhall/unixhelp/C_strings.html)
- Để thách thức bản thân, đọc về việc tối ưu hóa các hàm chuỗi: [https://opensource.com/article/19/5/how-write-good-c-main-function](https://opensource.com/article/19/5/how-write-good-c-main-function)

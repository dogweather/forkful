---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:06.358301-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Nội suy chuỗi cho phép chúng ta chèn trực tiếp các biến vào trong chuỗi. Điều này rất tiện lợi để tạo văn bản động, như tin nhắn cá nhân hoá hoặc dữ liệu được định dạng.

## Làm thế nào:

C không có tính năng nội suy chuỗi tích hợp sẵn, nhưng chúng ta có thể khéo léo sử dụng `sprintf` hoặc `snprintf`. Dưới đây là cách thực hiện:

```c
#include <stdio.h>

int main() {
    char name[] = "Alex";
    int age = 29;
    char message[50];

    snprintf(message, sizeof(message), "Xin chào, tôi là %s, và tôi %d tuổi.", name, age);
    printf("%s\n", message);  // Đầu ra mẫu: Xin chào, tôi là Alex, và tôi 29 tuổi.

    return 0;
}
```

## Sâu hơn nữa:

C luôn đòi hỏi bạn phải tự mình xử lý chuỗi một cách cẩn thận. Nội suy không phải là tính năng trực tiếp – thay vào đó, chúng ta sử dụng các dấu định vị và định dạng như `%s` cho chuỗi hoặc `%d` cho số trong các hàm như `printf` hoặc `sprintf`.

Trong lịch sử, các nhà phát triển ở các ngôn ngữ khác đã được cưng chiều với những tiện ích nội suy. Trong C#, bạn chỉ cần làm `var message = $"Hello, {name}!";`. Swift và Ruby cũng có những tiện ích riêng.

Quay lại với sân nhà C của chúng ta, các định dạng đóng vai trò quan trọng. Hãy nhớ rằng, `sprintf` có thể rủi ro nếu bạn xác định kích thước của bộ đệm không chính xác và bạn có thể làm tràn bộ đệm (`snprintf` an toàn hơn do giới hạn bộ đệm). Hơn nữa, việc quản lý này một cách thủ công có nghĩa là bạn có nhiều kiểm soát hơn - một nguyên tắc được yêu thích trong tinh thần của C.

Tuy nhiên, có nhiều cách để làm một việc: các thư viện bên thứ ba, như GLib, giới thiệu các tiện ích chuỗi tốt hơn. Các hàm như `g_strdup_printf` hoạt động tương tự như `sprintf` nhưng tự động quản lý việc cấp phát bộ nhớ cho bạn.

Các macros và hàm biến thiên cũng có thể mô phỏng nội suy, nhưng đó là một kỹ thuật nâng cao đòi hỏi sự hiểu biết vững chắc về macros và kiểu `va_list` từ `<stdarg.h>`.

## Xem thêm:

- Ngôn ngữ lập trình ISO/IEC — C: https://www.iso.org/standard/74528.html
- Chuẩn C11 (N1570): http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- Tài liệu của thư viện C GNU (glibc) về `printf`: https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html
- Các hàm tiện ích chuỗi của GLib: https://developer.gnome.org/glib/stable/glib-Strings.html

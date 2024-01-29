---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:57:51.261307-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển một chuỗi thành chữ thường có nghĩa là thay đổi tất cả các chữ in hoa sang các chữ thường tương ứng. Việc này được thực hiện nhằm đạt được sự nhất quán, giúp việc tìm kiếm, so sánh và sắp xếp hiệu quả hơn, nơi mà độ nhạy cảm với chữ hoa hay thường có thể gây rắc rối.

## Làm thế nào:

Trong C, bạn thường sẽ lặp qua chuỗi, chuyển đổi từng ký tự. Dưới đây là một ví dụ nhanh:

```c
#include <stdio.h>
#include <ctype.h>

void toLowercase(char *str) {
    if (!str) return; // Kiểm tra an toàn
    while (*str) {
        *str = tolower((unsigned char)*str); // Chuyển đổi từng ký tự sang chữ thường
        str++; // Chuyển sang ký tự tiếp theo
    }
}

int main() {
    char myStr[] = "Hello, World!";
    toLowercase(myStr);
    printf("%s\n", myStr); // Đầu ra: hello, world!
    return 0;
}
```

## Sâu hơn nữa

Ngày xưa, khi bộ nhớ máy tính còn nhỏ, mọi người quan tâm đến từng byte. Việc chuyển đổi chuỗi không phải là điều đơn giản; mặc định sử dụng một dạng chữ giúp tiết kiệm không gian. Bây giờ, nó ít liên quan đến không gian hơn, mà chủ yếu là về chức năng.

Tại sao sử dụng `tolower` mà không tự viết lẻ? Thư viện chuẩn của C đã có sẵn. Nó xử lý những điều bất thường trong các bộ ký tự và ngôn ngữ khác nhau. Tự viết lẻ? Có lẽ bạn sẽ bỏ sót các trường hợp ngoại lệ. Bên cạnh đó, sử dụng thư viện chuẩn có nghĩa là ít mã để duy trì hơn.

Điều thú vị: ASCII cũ có số 32 như là số ma thuật phân biệt các trường hợp chữ hoa và chữ thường—thêm hoặc bớt 32 để chuyển đổi giữa 'A' và 'a'. Với Unicode, mọi thứ không còn đơn giản nữa.

Các lựa chọn khác? Thư viện. Đối với lập trình viên C hiện đại, các thư viện như GLib biến đổi chuỗi trong nháy mắt, xử lý UTF-8 và những thứ khác, nhưng đó là quá mức để chỉ với chuỗi ASCII.

## Xem thêm

- Tham khảo Thư viện Chuẩn của C: <http://www.cplusplus.com/reference/cctype/tolower/>
- Bảng và Mô tả ASCII: <https://www.asciitable.com/>
- Thao tác Unicode với GLib: <https://developer.gnome.org/glib/stable/glib-Unicode-Manipulation.html>

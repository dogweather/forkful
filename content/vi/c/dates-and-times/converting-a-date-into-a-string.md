---
title:                "Chuyển đổi ngày thành chuỗi"
aliases:
- /vi/c/converting-a-date-into-a-string/
date:                  2024-02-03T17:55:05.217616-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi ngày thành chuỗi"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi một ngày thành chuỗi trong C bao gồm việc dịch một cấu trúc ngày tháng hoặc dấu thời gian thành định dạng dễ đọc cho con người. Các lập trình viên thường thực hiện tác vụ này để hiển thị các ngày trong nhật ký, giao diện người dùng, hoặc khi lưu trữ các ngày trong định dạng văn bản như JSON hoặc CSV.

## Cách thực hiện:

Hàm `strftime` từ thư viện `<time.h>` thường được sử dụng cho mục đích này. Nó cho phép bạn định dạng ngày và giờ theo nhiều cách khác nhau bằng cách chỉ định các chỉ thị định dạng. Dưới đây là một ví dụ nhanh:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Chuyển đổi ngày & giờ thành chuỗi (ví dụ, "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Ngày và Giờ Hiện Tại: %s\n", dateStr);
    return 0;
}
```

Đầu ra mẫu có thể trông như thế này:

```
Ngày và Giờ Hiện Tại: Wed Jun 30 21:49:08 2021
```

Bạn có thể tùy chỉnh định dạng bằng cách thay đổi các chỉ thị định dạng được truyền vào `strftime`. Ví dụ, để lấy ngày theo định dạng `YYYY-MM-DD`, bạn sẽ sử dụng `"%Y-%m-%d"`.

## Tìm hiểu sâu hơn

Hàm `strftime` và thư viện `<time.h>` là một phần của Thư viện Tiêu Chuẩn C, có từ tiêu chuẩn ANSI C ban đầu (C89/C90). Mặc dù phương pháp này đơn giản và được hỗ trợ trên nhiều nền tảng, nhưng so với các ngôn ngữ lập trình hiện đại cung cấp các thư viện ngày và giờ trực quan hơn, phương pháp này có thể trông thấp cấp và gượng gạo hơn.

Cần lưu ý, mặc dù các hàm về thời gian của thư viện tiêu chuẩn C được hỗ trợ rộng rãi và tương đối đơn giản để sử dụng, chúng thiếu một số tính năng phức tạp về thao tác múi giờ và quốc tế hóa có trong thư viện của các ngôn ngữ mới hơn hoặc trong các thư viện C của bên thứ ba như International Components for Unicode (ICU).

Tuy nhiên, khả năng tùy chỉnh của hàm `strftime` và sự hỗ trợ rộng rãi trên nhiều nền tảng làm cho nó trở thành một công cụ đáng tin cậy và hữu ích cho việc chuyển đổi chuỗi ngày trong C. Các lập trình viên đến từ ngôn ngữ với thư viện datetime cấp cao hơn có thể cần thích nghi với tính chất thấp cấp của nó nhưng sẽ thấy nó cực kỳ mạnh mẽ và linh hoạt cho việc định dạng ngày và giờ cho nhiều ứng dụng khác nhau.

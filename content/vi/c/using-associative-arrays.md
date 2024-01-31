---
title:                "Sử dụng mảng liên kết"
date:                  2024-01-30T19:10:35.324014-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, hay bản đồ hash, là các cặp khóa-giá trị cho phép bạn lưu trữ và truy xuất dữ liệu với một khóa. Chúng vô cùng hữu ích trong C bởi vì chúng cho phép truy cập dữ liệu nhanh hơn so với danh sách, đặc biệt là khi bạn đang xử lý một lượng lớn dữ liệu.

## Làm thế nào:

C không có hỗ trợ tích hợp cho mảng kết hợp như một số ngôn ngữ khác, nhưng chúng ta có thể sử dụng cấu trúc và một số hàm thư viện để có được chức năng tương tự. Dưới đây là một cài đặt đơn giản sử dụng thư viện `uthash`, mà bạn cần phải bao gồm trong dự án của mình.

Đầu tiên, định nghĩa một cấu trúc để giữ các cặp khóa-giá trị:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Đây sẽ là khóa của chúng ta
    char name[10]; // Đây là giá trị được kết hợp với khóa của chúng ta
    UT_hash_handle hh; // Làm cho cấu trúc này có thể băm
} nguoi;
```

Tiếp theo, hãy thêm một số mục và truy xuất chúng:

```C
int main() {
    nguoi *con_nguoi_cua_toi = NULL, *s;

    // Thêm một mục
    s = (nguoi*)malloc(sizeof(nguoi));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(con_nguoi_cua_toi, id, s);

    // Truy xuất một mục
    int user_id = 1;
    HASH_FIND_INT(con_nguoi_cua_toi, &user_id, s);
    if (s) {
        printf("Tìm thấy: %s\n", s->name);
    }
    
    return 0;
}
```

Kết quả mẫu sẽ là:

```
Tìm thấy: Alice
```

Đừng quên giải phóng bộ nhớ đã phân bổ và giải phóng bảng băm khi hoàn thành để tránh rò rỉ bộ nhớ.

## Tìm hiểu sâu

Mặc dù mảng kết hợp không phải là bản địa của C, các thư viện như `uthash` lấp đầy khoảng trống khá tốt, cung cấp một cách khá thẳng thắn để sử dụng chức năng này. Trong lịch sử, các nhà phát triển C phải triển khai phiên bản riêng của họ về các cấu trúc dữ liệu này, dẫn đến các triển khai đa dạng và thường xuyên phức tạp, đặc biệt là cho những người mới bắt đầu với ngôn ngữ này.

Hãy nhớ rằng, hiệu quả khi sử dụng mảng kết hợp trong C phụ thuộc rất nhiều vào việc hàm băm phân phối giá trị trên bảng một cách đều đặn nhằm giảm thiểu va chạm. Trong khi các thư viện như `uthash` cung cấp một sự cân bằng tốt về dễ sử dụng và hiệu suất, trong các ứng dụng quan trọng mà hiệu suất là yếu tố hàng đầu, bạn có thể muốn tùy chỉnh hoặc triển khai bảng băm của riêng mình.

Đối với các ứng dụng yêu cầu hiệu suất tối đa, cấu trúc dữ liệu thay thế hoặc thậm chí là ngôn ngữ lập trình khác với sự hỗ trợ tích hợp cho mảng kết hợp có thể là lựa chọn tốt hơn. Tuy nhiên, trong nhiều tình huống, đặc biệt là khi bạn đã làm việc trong môi trường C, việc sử dụng một thư viện như `uthash` cung cấp một sự cân bằng thực tế giữa hiệu suất và tiện lợi.

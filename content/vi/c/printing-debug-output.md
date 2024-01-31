---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:04:56.678775-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

In thông tin gỡ lỗi giống như việc chèn các điểm kiểm tra nhỏ vào trong mã của bạn để phun ra thông tin, giúp xác định xem điều gì đang xảy ra bên trong đó. Lập trình viên làm điều này để xác định lỗi hoặc để đảm bảo mã của họ thực hiện đúng như mong đợi, từng bước một.

## Làm thế nào:

Đây là vấn đề—in thông tin gỡ lỗi rất dễ dàng. Công cụ chính trong C là `printf`. Xem ví dụ đơn giản sau:

```c
#include <stdio.h>

int main() {
    int loopCounter = 0;
    for(loopCounter = 0; loopCounter < 5; loopCounter++) {
        printf("Lần lặp: %d\n", loopCounter);
        // Mã phức tạp hơn ở đây.
    }
    printf("Vòng lặp kết thúc.\n");
    // Phần còn lại của mã của bạn.
    return 0;
}
```

Chạy đoạn mã này sẽ hiển thị trên màn hình của bạn:

```
Lần lặp: 0
Lần lặp: 1
Lần lặp: 2
Lần lặp: 3
Lần lặp: 4
Vòng lặp kết thúc.
```

Đơn giản, phải không? Chỉ cần nhớ xóa hoặc ghi chú những dòng này khi bạn hoàn tất để bảng điều khiển của bạn không bị lộn xộn.

## Sâu hơn nữa

Ngày xưa, không có bất kỳ Trình gỡ lỗi Môi trường Phát triển Tích hợp (IDE) nào để hỗ trợ bạn. Đầu ra thô tới terminal là những gì bạn có. Ngày nay, nó vẫn là vàng cho việc chẩn đoán nhanh và bẩn.

Có phương án thay thế không? Chà, cho việc gỡ lỗi nặng nề, bạn có thể chuyển sang sử dụng trình gỡ lỗi của IDE chính thức, hoặc các tiện ích đăng nhập cung cấp nhiều quyền kiểm soát hơn.

`printf` là lựa chọn của bạn, nhưng có nhiều điều hơn dưới lớp vỏ. Ví dụ, `fprintf(stderr, ...)` có thể chuyển hướng thông điệp của bạn đến luồng lỗi chuẩn, làm cho chúng dễ phân biệt hơn so với đầu ra chuẩn.

Ngoài ra, nếu hiệu suất là quan trọng, bạn có thể tránh ghi nhật ký trong các vòng lặp chặt chẽ hoặc xem xét biên dịch với các macro cho phép bạn loại bỏ mã gỡ lỗi trong sản phẩm.

## Xem thêm

- [GNU Debugger (GDB)](https://www.gnu.org/software/gdb/) cho khi bạn sẵn sàng để vượt qua `printf`.
- [Thư viện Ghi nhật ký C](https://www.slant.co/topics/1183/~best-logging-add-ons-for-c-programming) cho việc ghi nhật ký có cấu trúc.
- [Học C Cách Khó Khăn](https://learncodethehardway.org/c/) cho một cái nhìn sâu hơn vào thế giới rộng lớn của lập trình C.

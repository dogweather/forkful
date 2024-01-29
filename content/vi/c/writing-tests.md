---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:13:17.295273-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết các bài kiểm tra có nghĩa là tạo ra những đoạn code kiểm tra xem đoạn code khác của bạn có hoạt động không. Lập trình viên làm điều này để phát hiện lỗi sớm, tiết kiệm thời gian và đảm bảo rằng code hoạt động ngay bây giờ và sẽ tiếp tục hoạt động sau này.

## Làm thế nào:

Trong C, bạn có thể viết các bài kiểm tra bằng cách sử dụng một framework kiểm tra như CUnit hoặc tự cuốn một thiết lập kiểm tra đơn giản của riêng bạn. Dưới đây là một ví dụ cơ bản sử dụng assert để tạo một hàm kiểm tra cơ bản cho hàm `add`.

```C
#include <assert.h>

// Hàm để kiểm tra
int add(int a, int b) {
    return a + b;
}

// Hàm kiểm tra
void test_add() {
    assert(add(2, 2) == 4);
    assert(add(-1, 1) == 0);
}

int main() {
    test_add();
    printf("Tất cả các bài kiểm tra đã qua!\n");
    return 0;
}
```

Đầu ra mẫu, nếu tất cả các bài kiểm tra đều qua:

```
Tất cả các bài kiểm tra đã qua!
```

Nếu một bài kiểm tra thất bại, chương trình sẽ bị hủy và in ra thông báo lỗi.

## Sâu hơn

Lịch sử, C không đi kèm với một framework kiểm tra tích hợp. Lập trình viên thường viết các hàm kiểm tra tùy chỉnh hoặc sử dụng các framework của bên thứ ba. Các framework phổ biến bao gồm CUnit, Check và Unity. Mỗi framework đều cung cấp các tính năng như khám phá bài kiểm tra tự động, quy trình thiết lập/dỡ bỏ và báo cáo kết quả kiểm tra. Đối với các dự án nhỏ, các bài kiểm tra đơn giản dựa trên assert có thể đủ, nhưng khi độ phức tạp tăng lên, một framework phù hợp giúp tiết kiệm thời gian và giảm phiền toái.

## Xem thêm

Dưới đây là một số liên kết hữu ích để khám phá sâu hơn:

- [CUnit](http://cunit.sourceforge.net/)
- [Check](https://libcheck.github.io/check/)
- [Unity](http://www.throwtheswitch.org/unity)
- [Assert.h trong lập trình C](https://www.tutorialspoint.com/assert-h-in-c-programming)

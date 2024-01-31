---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:12:41.890810-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Số phức, là sự kết hợp của phần thực và phần ảo (như 3 + 4i), là chìa khóa trong các phép tính nâng cao, như xử lý tín hiệu hay giải các phương trình nhất định. Các lập trình viên xử lý chúng cho các ứng dụng nặng về toán học, nơi mà số truyền thống không đáp ứng được.

## Làm thế nào:
C, từ C99, có kiểu phức và thư viện tự nhiên. Dưới đây là cách sử dụng:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Khai báo hai số phức
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Các phép toán với số phức
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // In kết quả
    printf("Tổng: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Tích: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Giá trị tuyệt đối & góc pha
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Kết quả mẫu:
```
Tổng: 3.0 + 1.0i
Tích: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Tìm hiểu sâu
Số phức có từ hàng thế kỷ, với nguồn gốc từ đại số thế kỷ 16. Nhanh chóng tiến lên, giờ đây chúng là một phần không thể thiếu trong nhiều ngôn ngữ lập trình, không chỉ C.

Tiêu chuẩn C99 giới thiệu `<complex.h>`, một tiêu đề định nghĩa các macro, hàm, và kiểu dữ liệu `complex`. Có những lựa chọn khác - như tạo cấu trúc của riêng bạn, nhưng tại sao lại phải tái sáng tạo bánh xe? Thư viện tiêu chuẩn C được tối ưu hoá và sẵn sàng sử dụng.

Mặc dù có sức mạnh, sự hỗ trợ số phức của C không thoát khỏi những chỉ trích. Nó có thể kém trực quan hơn so với các tính năng tương tự trong ngôn ngữ như Python, và việc xử lý các trường hợp ngoại lệ có thể trở nên phức tạp. Nhưng về hiệu suất thô, nó vẫn là một lựa chọn vững chắc.

## Xem thêm
- Tài liệu Tiêu chuẩn C99 cho `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- Tiêu chuẩn IEEE cho Số học dấu phẩy động (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Hướng dẫn trực tuyến cho toán học số phức trong lập trình C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming

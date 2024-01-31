---
title:                "Làm tròn số"
date:                  2024-01-28T22:06:36.756196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số là việc loại bỏ các chữ số sau một điểm nhất định và tùy chọn điều chỉnh chữ số cuối cùng được giữ lại. Các lập trình viên làm tròn để giảm độ chính xác khi các giá trị chính xác không cần thiết, quản lý lỗi dấu phẩy động, hoặc chuẩn bị số cho hiển thị thân thiện với người dùng.

## Làm thế nào:
Trong C, bạn thường dùng các hàm `floor()`, `ceil()`, hoặc `round()`. Dưới đây là các ví dụ nhanh:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

Để kiểm soát nhiều hơn, như làm tròn đến một vị trí cụ thể, bạn nhân, làm tròn, và chia:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Làm tròn đến 2 chữ số thập phân: %.2f\n", num_rounded); // Làm tròn đến 2 chữ số thập phân: 3.14
```

## Sâu hơn
Ngày xưa, làm tròn thường có nghĩa là quá trình thủ công - một công việc nặng nhọc chỉ với bút và giấy. Với máy tính, chúng ta đã tự động hóa điều này, nhưng số học dấu phẩy động đã mang lại những subtlety do bản chất nhị phân của nó, nơi mà một số số không thể được biểu diễn một cách chính xác.

Các phương pháp thay thế cho việc làm tròn tiêu chuẩn bao gồm cắt (đơn giản là bỏ đi các chữ số thừa) hoặc làm tròn kiểu ngân hàng, làm tròn về số chẵn gần nhất khi chính xác ở giữa hai giá trị, giảm thiểu sự thiên vị trong các tính toán lặp lại.

Việc thực hiện trở nên phức tạp khi bạn cần làm tròn số với độ chính xác tùy ý hoặc xử lý các trường hợp đặc biệt như vô cùng, NaN có tín hiệu, hoặc các giá trị subnormal. Các hàm thư viện chuẩn của C xử lý các điều cơ bản, nhưng nếu bạn cần làm tròn số thập phân theo cách tùy chỉnh, bạn sẽ cần nhiều hơn `math.h`.

## Xem thêm
- [Tài liệu `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Số học dấu phẩy động](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [Những khó khăn khi xác minh tính toán dấu phẩy động](https://dl.acm.org/doi/10.1145/1186736.1186737)

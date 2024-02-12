---
title:                "Làm tròn số"
aliases: - /vi/c/rounding-numbers.md
date:                  2024-02-03T18:08:09.496667-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/rounding-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm tròn số là quá trình điều chỉnh các chữ số của một số để giảm độ chính xác theo những quy tắc nhất định, hoặc về phía số nguyên gần nhất hoặc một số lượng chữ số thập phân được chỉ định. Lập trình viên thực hiện điều này vì nhiều lý do, từ việc giới hạn lượng bộ nhớ cần thiết, đến việc đơn giản hóa đầu ra cho người dùng, hoặc đảm bảo các phép toán toán học chính xác nhạy cảm với những biến động nhỏ.

## Làm thế nào:

Làm tròn số trong C có thể được hoàn thành bằng cách sử dụng các hàm khác nhau, nhưng phương pháp phổ biến nhất bao gồm việc sử dụng các hàm `floor()`, `ceil()`, và `round()`. Những hàm này là một phần của thư viện toán học chuẩn, vì vậy bạn sẽ cần phải bao gồm `math.h` trong chương trình của mình.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Sử dụng floor() để làm tròn xuống
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Sử dụng ceil() để làm tròn lên
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Sử dụng round() để làm tròn đến số nguyên gần nhất
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Làm tròn đến một số lượng chữ số thập phân được chỉ định liên quan đến phép nhân và chia
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Làm tròn đến hai chữ số thập phân: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Đầu ra:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Làm tròn đến hai chữ số thập phân: 9.53
```

## Đi sâu hơn

Làm tròn số có bản chất lịch sử sâu rộng trong toán học và tính toán, thiết yếu cho cả khía cạnh lý thuyết và ứng dụng. Trong C, trong khi `floor()`, `ceil()`, và `round()` cung cấp chức năng cơ bản, bản chất của việc làm tròn số thực sang số nguyên hoặc số chữ số thập phân cụ thể còn tinh tế hơn do biểu diễn nhị phân của số thực chấm động. Biểu diễn này có thể dẫn đến kết quả không mong đợi do cách xử lý các số không thể được biểu diễn chính xác trong nhị phân (như 0.1).

Những hàm này là một phần của thư viện chuẩn C, được định nghĩa trong `<math.h>`. Khi làm tròn số, đặc biệt là cho các tính toán tài chính hoặc kỹ thuật chính xác, cần phải xem xét tác động của việc sử dụng số thực chấm động nhị phân. Các phương án thay thế cho các hàm C tích hợp để làm tròn chính xác cao hoặc cụ thể vào số thập phân có thể bao gồm việc thực hiện các hàm làm tròn tùy chỉnh hoặc sử dụng thư viện được thiết kế cho các phép toán số học độ chính xác tùy ý, như GMP hoặc MPFR, mặc dù những điều này giới thiệu thêm độ phức tạp và phụ thuộc.

Trên thực tế, việc chọn phương pháp làm tròn phù hợp trong C đòi hỏi cân nhắc giữa nhu cầu về độ chính xác, hiệu suất và tính thiết thực, với sự hiểu biết sâu sắc về các yêu cầu cụ thể của lĩnh vực ứng dụng đang được phát triển.

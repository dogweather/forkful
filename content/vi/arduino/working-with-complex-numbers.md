---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:12:20.770940-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo, thường được viết là `a + bi`. Chúng rất quan trọng cho một số dự án Arduino nặng về toán học liên quan đến xử lý tín hiệu, kỹ thuật điện, hoặc bất kỳ lĩnh vực nào khác mà hiện tượng được mô hình hóa tốt nhất trong một mặt phẳng.

## Làm thế nào:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Bắt đầu giao tiếp nối tiếp
  
  Complex myComplex(2, 3); // Tạo một số phức 2 + 3i
  Complex anotherComplex(1, 1); // Tạo một số phức khác 1 + 1i
  
  // Phép cộng
  Complex result = myComplex + anotherComplex; 
  Serial.print("Phép cộng: "); 
  result.print(); // Xuất ra 3 + 4i
  
  // Phép nhân
  result = myComplex * anotherComplex; 
  Serial.print("Phép nhân: ");
  result.print(); // Xuất ra -1 + 5i
}

void loop() {
  // Không được sử dụng trong ví dụ này
}
```
Kết quả mẫu:
```
Phép cộng: 3 + 4i
Phép nhân: -1 + 5i
```

## Sâu hơn
Ban đầu, số phức gặp phải sự hoài nghi, nhưng chúng đã trở thành trung tâm trong nhiều lĩnh vực khoa học. Lịch sử, chúng được công nhận vì cung cấp giải pháp cho các phương trình đa thức thiếu giải pháp thực.

Arduino không bao gồm số phức trong thư viện tiêu chuẩn của mình, nhưng bạn có thể tận dụng các thư viện như `Complex.h` để xử lý chúng. Nội bộ, các thư viện này định nghĩa một lớp Complex, thường sử dụng hai số double để lưu trữ phần thực và phần ảo, và nạp chồng các toán tử để hỗ trợ phép tính.

Như một giải pháp thay thế, đối với các ứng dụng không cần thiết phải có phép toán số phức, xem xét sử dụng các chiến lược toán học hoặc thư viện khác. Tuy nhiên, nhớ là sử dụng số dấu phẩy động thay vì số phức có thể làm đơn giản hóa quá mức một số vấn đề.

## Xem Thêm
- Thư viện [Complex.h](https://github.com/RobTillaart/Complex) của Rob Tillaart.
- Sâu hơn vào [toán học đằng sau số phức](https://mathworld.wolfram.com/ComplexNumber.html).

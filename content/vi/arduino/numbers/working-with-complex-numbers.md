---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:20.770940-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ban \u0111\u1EA7u, s\u1ED1 ph\u1EE9c g\u1EB7\
  p ph\u1EA3i s\u1EF1 ho\xE0i nghi, nh\u01B0ng ch\xFAng \u0111\xE3 tr\u1EDF th\xE0\
  nh trung t\xE2m trong nhi\u1EC1u l\u0129nh v\u1EF1c khoa h\u1ECDc. L\u1ECBch s\u1EED\
  , ch\xFAng \u0111\u01B0\u1EE3c c\xF4ng nh\u1EADn v\xEC\u2026"
lastmod: '2024-04-05T22:50:51.294684-06:00'
model: gpt-4-0125-preview
summary: "Ban \u0111\u1EA7u, s\u1ED1 ph\u1EE9c g\u1EB7p ph\u1EA3i s\u1EF1 ho\xE0i\
  \ nghi, nh\u01B0ng ch\xFAng \u0111\xE3 tr\u1EDF th\xE0nh trung t\xE2m trong nhi\u1EC1\
  u l\u0129nh v\u1EF1c khoa h\u1ECDc."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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

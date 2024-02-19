---
aliases:
- /vi/cpp/organizing-code-into-functions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:26.330432-07:00
description: "Chia code th\xE0nh c\xE1c h\xE0m ngh\u0129a l\xE0 t\xE1ch code c\u1EE7\
  a b\u1EA1n th\xE0nh c\xE1c ph\u1EA7n nh\u1ECF h\u01A1n, c\xF3 th\u1EC3 t\xE1i s\u1EED\
  \ d\u1EE5ng. Ch\xFAng ta l\xE0m nh\u01B0 v\u1EADy \u0111\u1EC3 tr\xE1nh l\u1EB7\
  p l\u1EA1i, l\xE0m cho code c\u1EE7a\u2026"
lastmod: 2024-02-18 23:08:51.050158
model: gpt-4-0125-preview
summary: "Chia code th\xE0nh c\xE1c h\xE0m ngh\u0129a l\xE0 t\xE1ch code c\u1EE7a\
  \ b\u1EA1n th\xE0nh c\xE1c ph\u1EA7n nh\u1ECF h\u01A1n, c\xF3 th\u1EC3 t\xE1i s\u1EED\
  \ d\u1EE5ng. Ch\xFAng ta l\xE0m nh\u01B0 v\u1EADy \u0111\u1EC3 tr\xE1nh l\u1EB7\
  p l\u1EA1i, l\xE0m cho code c\u1EE7a\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Chia code thành các hàm nghĩa là tách code của bạn thành các phần nhỏ hơn, có thể tái sử dụng. Chúng ta làm như vậy để tránh lặp lại, làm cho code của mình dễ đọc hơn, và đơn giản hóa việc gỡ lỗi và kiểm thử. Các hàm được sắp xếp tốt có thể giống như việc có một hộp công cụ được ghi nhãn gọn gàng, sẵn sàng sử dụng và chia sẻ.

## Làm Thế Nào:
Hãy lấy một nhiệm vụ phổ biến: tính diện tích của một hình tròn. Thay vì viết cùng một công thức mỗi lần, chúng ta gói gọn nó vào một hàm.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Diện tích hình tròn với bán kính " << r << " là " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Kết quả mẫu:
```
Diện tích hình tròn với bán kính 5 là 78.5397
```

## Đào Sâu
Truyền thống, các thủ tục và hàm đã là xương sống của lập trình có cấu trúc, được ca ngợi vào những năm 1960 để chống lại các vấn đề của "mã spaghetti" trong các ngôn ngữ lập trình mệnh lệnh trước đó. Các phương án thay thế như OOP (Lập trình Hướng Đối Tượng) đẩy xa hơn bằng cách kết hợp các hàm này với cấu trúc dữ liệu. Trong C++, bạn có các hàm thông thường, phương thức lớp (bao gồm phương thức tĩnh), lambdas, và hàm mẫu, mỗi cái đều cung cấp những lợi ích khác nhau. Việc thực hiện các hàm được tổ chức tốt thường liên quan đến việc tuân theo các nguyên tắc như DRY ("Don't Repeat Yourself" - Đừng Lặp Lại Chính Mình) và SRP (Single Responsibility Principle - Nguyên Tắc Trách Nhiệm Đơn), nghĩa là mỗi hàm chỉ làm một việc và làm tốt việc đó.

## Xem Thêm
Để biết thêm về các hàm trong C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Về các nguyên tắc thiết kế liên quan đến hàm:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Tìm hiểu về lambdas và sử dụng hàm nâng cao:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures

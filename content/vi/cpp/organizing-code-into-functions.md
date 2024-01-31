---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:26.330432-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

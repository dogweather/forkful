---
title:                "Làm việc với số phức"
aliases:
- /vi/cpp/working-with-complex-numbers/
date:                  2024-01-28T22:12:16.993768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Số phức mở rộng số thực bằng cách thêm một đơn vị ảo, biểu diễn như là 'i', nơi mà i^2 = -1. Lập trình viên sử dụng chúng cho các bài toán mô phỏng, xử lý tín hiệu, và giải các bài toán toán học đòi hỏi làm việc trong không gian hai chiều.

## Cách thực hiện:
C++ có một thư viện tích hợp `<complex>` giúp dễ dàng làm việc với số phức. Dưới đây là cái nhìn nhanh:

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // Tạo một số phức (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // Một số phức khác (3 + 4i)

    // Phép cộng
    std::complex<double> kết_quả = num1 + num2;
    std::cout << "Kết quả cộng: " << kết_quả << std::endl; // (5 + 7i)

    // Phép nhân
    kết_quả = num1 * num2;
    std::cout << "Kết quả nhân: " << kết_quả << std::endl; // (-6 + 17i)

    // Số liên hợp
    kết_quả = std::conj(num1);
    std::cout << "Số liên hợp của num1: " << kết_quả << std::endl; // (2 - 3i)
    
    return 0;
}
```

## Sâu hơn
Số phức có một lịch sử phong phú, lần đầu tiên xuất hiện trong các giải pháp của phương trình bậc ba vào thế kỷ 16. Chúng thiết yếu trong nhiều lĩnh vực, không chỉ lập trình. Trong khoa học máy tính, số phức giúp trong các thuật toán yêu cầu không gian số hai chiều, như Chuyển đổi Fourier Nhanh (FFT).

Mặc dù thư viện `<complex>` của C++ là tiêu chuẩn, nhưng cũng tồn tại các lựa chọn khác trong các ngôn ngữ khác, như kiểu dữ liệu `complex` của Python hay thư viện toán học của JavaScript. Chính thư viện `<complex>` cung cấp chức năng toàn diện, bao gồm các phép toán lượng giác, mũ, và logarit được thiết kế riêng cho số phức.

Khi lập trình với số phức, việc hiểu biết về toán học cơ bản là chìa khóa để tránh các thiếu chính xác và hiểu các hoạt động như liên hợp phức, nghịch đảo dấu của phần ảo, hoặc các hệ quả của công thức Euler liên hệ giữa mũ phức và hàm lượng giác.

## Xem thêm
- Tài liệu về Thư Viện Mẫu Chuẩn C++: https://en.cppreference.com/w/cpp/header/complex
- Khám phá sâu hơn về toán học của số phức: https://mathworld.wolfram.com/ComplexNumber.html
- Để trực quan hóa, thư viện Python Matplotlib có thể vẽ số phức: https://matplotlib.org/

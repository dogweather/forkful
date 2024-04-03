---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:09.033437-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C, s\u1ED1 ph\u1EE9c \u0111\u01B0\u1EE3\
  c h\u1ED7 tr\u1EE3 b\u1EDFi Th\u01B0 vi\u1EC7n Ti\xEAu chu\u1EA9n, c\u1EE5 th\u1EC3\
  \ l\xE0 `<complex.h>`. \u0110\u1EC3 s\u1EED d\u1EE5ng ch\xFAng, khai b\xE1o c\xE1\
  c bi\u1EBFn v\u1EDBi ki\u1EC3u `double complex`\u2026"
lastmod: '2024-03-13T22:44:37.259379-06:00'
model: gpt-4-0125-preview
summary: "Trong C, s\u1ED1 ph\u1EE9c \u0111\u01B0\u1EE3c h\u1ED7 tr\u1EE3 b\u1EDF\
  i Th\u01B0 vi\u1EC7n Ti\xEAu chu\u1EA9n, c\u1EE5 th\u1EC3 l\xE0 `<complex.h>`."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Trong C, số phức được hỗ trợ bởi Thư viện Tiêu chuẩn, cụ thể là `<complex.h>`. Để sử dụng chúng, khai báo các biến với kiểu `double complex` (hoặc `float complex` cho độ chính xác đơn). Dưới đây là cách thực hiện các phép toán cơ bản:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Khai báo một số phức 1+2i
    double complex z2 = 1.0 - 2.0*I; // Khai báo một số phức khác 1-2i
    
    // Cộng
    double complex sum = z1 + z2;
    printf("Tổng: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Đầu ra: Tổng: 2.00 + 0.00i

    // Nhân
    double complex product = z1 * z2;
    printf("Tích: %.2f + %.2fi\n", creal(product), cimag(product)); // Đầu ra: Tích: 5.00 + 0.00i

    // Liên hợp
    double complex conjugate = conj(z1);
    printf("Liên hợp của z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Đầu ra: Liên hợp của z1: 1.00 - 2.00i
    
    // Độ lớn
    double magnitude = cabs(z1);
    printf("Độ lớn của z1: %.2f\n", magnitude); // Đầu ra: Độ lớn của z1: 2.24

    // Pha
    double phase = carg(z1);
    printf("Pha của z1: %.2f\n", phase); // Đầu ra bằng radian
    
    return 0;
}
```
Lưu ý rằng `I` là một hằng số biểu thị đơn vị ảo trong `<complex.h>`. Các hàm như `creal()` và `cimag()` được sử dụng để trích xuất phần thực và phần ảo tương ứng, trong khi `conj()` tính toán liên hợp phức. Đối với độ lớn và pha (arg) của số phức, `cabs()` và `carg()` được sử dụng.

## Sâu hơn
Việc hỗ trợ số phức trong C tương đối mới, được chuẩn hóa trong C99. Trước đó, việc tính toán số phức trong C khá cồng kềnh, thường yêu cầu cấu trúc dữ liệu và hàm tùy chỉnh. Sự bao gồm của `<complex.h>` và các loại dữ liệu phức đã tăng cường đáng kể khả năng của ngôn ngữ cho các ứng dụng khoa học và kỹ thuật. Tuy nhiên, đáng chú ý là một số ngôn ngữ, như Python, cung cấp hỗ trợ trực quan hơn cho số phức thông qua các kiểu dữ liệu tích hợp và bộ thư viện phong phú hơn. Mặc dù vậy, hiệu suất và kiểm soát mà C cung cấp khiến nó trở thành lựa chọn ưa thích cho các nhiệm vụ tính toán hiệu suất cao, ngay cả khi điều này có nghĩa là phải đối mặt với cú pháp tương đối dài dòng hơn cho số học phức.

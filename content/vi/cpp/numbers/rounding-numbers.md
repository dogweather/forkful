---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:58.276399-07:00
description: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh gi\xE1\
  \ tr\u1ECB t\u1EDBi s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c \u0111\u1ED9\
  \ ch\xEDnh x\xE1c \u0111\xE3 ch\u1EC9 \u0111\u1ECBnh. L\u1EADp tr\xECnh vi\xEAn\
  \ th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u01A1n gi\u1EA3\
  n h\xF3a, ph\xF9 h\u1EE3p\u2026"
lastmod: '2024-03-13T22:44:37.036327-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh gi\xE1\
  \ tr\u1ECB t\u1EDBi s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c \u0111\u1ED9\
  \ ch\xEDnh x\xE1c \u0111\xE3 ch\u1EC9 \u0111\u1ECBnh."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cái gì & Tại sao?
Làm tròn số nghĩa là điều chỉnh giá trị tới số nguyên gần nhất hoặc độ chính xác đã chỉ định. Lập trình viên thực hiện điều này để đơn giản hóa, phù hợp với các ràng buộc thực tế, hoặc cải thiện hiệu suất bằng cách loại bỏ độ chính xác dư thừa.

## Làm thế nào:
C++ cung cấp một vài cách để làm tròn số, như `floor()`, `ceil()`, và `round()`:

```C++
#include <iostream>
#include <cmath> // cho các hàm làm tròn

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Đầu ra: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Đầu ra: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Đầu ra: round: 3

    // Để làm tròn với độ chính xác cố định, như làm tròn đến hai chữ số thập phân:
    double precise_num = 3.146;
    double bội số = 100.0;
    double làm tròn = std::round(precise_num * bội số) / bội số;

    std::cout << "làm tròn đến hai chữ số thập phân: " << làm tròn << "\n"; // Đầu ra: làm tròn đến hai chữ số thập phân: 3.15

    return 0;
}
```

## Đi Sâu Hơn
Trước C++11, làm tròn phụ thuộc vào các kỹ thuật thủ công hoặc các thư viện không chuẩn. Ngày nay, `<cmath>` cung cấp các phương pháp mạnh mẽ. `floor()` làm tròn xuống, `ceil()` làm tròn lên, trong khi `round()` chuyển đến số nguyên gần nhất, thậm chí xử lý việc phân giải tie-breaking (trường hợp 0.5) bằng cách làm tròn đến số chẵn.

Việc hiểu rõ hành vi của những hàm này là rất quan trọng; ví dụ, số âm có thể khiến bạn lúng túng (`std::round(-2.5)` tạo ra `-2.0`).

Có sự thay thế? Ép kiểu thành int sau khi cộng thêm 0.5 cho số dương là một mẹo cổ điển nhưng sai lệch với số âm và không phụ thuộc vào kiểu. Các thư viện như Boost có thể cung cấp các cách tiếp cận tinh tế hơn, trong khi các mở rộng ngôn ngữ hoặc đặc tính nội bộ của trình biên dịch có thể tối ưu hóa cho phần cứng cụ thể.

## Xem Thêm
- Tài liệu tham khảo C++ cho `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- Tiêu chuẩn IEEE cho Số Học Dấu Phẩy Động (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Thư viện Chuyển Đổi Số Học của Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/

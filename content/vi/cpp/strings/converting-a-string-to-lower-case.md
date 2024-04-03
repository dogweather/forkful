---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:55.521084-07:00
description: "C\xE1ch th\u1EE9c: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1\
  n lo\u1EA1i b\u1ECF s\u1EF1 kh\xE1c bi\u1EC7t v\u1EC1 ch\u1EEF hoa ch\u1EEF th\u01B0\
  \u1EDDng trong C++, c\xE1c ch\u1EEF c\xE1i in hoa ch\xE0o thua tr\u01B0\u1EDBc c\xE1\
  c ch\u1EEF nh\u1ECF."
lastmod: '2024-03-13T22:44:37.025567-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n lo\u1EA1i b\u1ECF s\u1EF1\
  \ kh\xE1c bi\u1EC7t v\u1EC1 ch\u1EEF hoa ch\u1EEF th\u01B0\u1EDDng trong C++, c\xE1\
  c ch\u1EEF c\xE1i in hoa ch\xE0o thua tr\u01B0\u1EDBc c\xE1c ch\u1EEF nh\u1ECF."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thức:
Dưới đây là cách bạn loại bỏ sự khác biệt về chữ hoa chữ thường trong C++, các chữ cái in hoa chào thua trước các chữ nhỏ:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string origText = "C++ makes me Shout!";
    std::string lowerText = origText;

    std::transform(origText.begin(), origText.end(), lowerText.begin(), 
                   [](unsigned char c) { return std::tolower(c); });

    std::cout << "Bản gốc: " << origText << std::endl;
    std::cout << "Chữ thường: " << lowerText << std::endl;
    
    return 0;
}
```
Kết quả:
```
Bản gốc: C++ makes me Shout!
Chữ thường: c++ makes me shout!
```

## Sâu hơn nữa
Ngày xưa, trước khi `std::transform` và lambdas xuất hiện, người ta thường lặp qua từng ký tự và chuyển chúng thành chữ thường một cách thủ công - một công việc cực nhọc hơn một chút. Tuy nhiên, việc sử dụng `std::transform` với `std::tolower` là hiệu quả và ít dễ phạm lỗi, mặc dù, đối với C++, vẫn tồn tại các cách khác. Lưu ý về địa điểm: Hành vi của `std::tolower` có thể thay đổi. Nếu dự án của bạn cần Unicode, hãy xem xét các thư viện bên thứ ba như ICU, được xây dựng cho một sân khấu toàn cầu.

Cũng đáng chú ý là sự bổ sung của C++20, `std::ranges::transform`, mang lại các biến đổi dựa vào phạm vi, làm cho cú pháp thêm hấp dẫn và tuân thủ triết lý 'range' rằng việc lập trình nên trực quan và ít dễ mắc lỗi hơn.

Về chi tiết triển khai, mỗi ký tự có một giá trị ASCII và sự khác biệt giữa chữ thường và chữ hoa là nhất quán. Các biến đổi nhìn vào các giá trị này để chuyển chúng thành chữ thường - cơ bản là chơi trò limbo số học.

## Xem thêm
Dành cho những người tò mò muốn biết thêm:

- Tài liệu tham khảo C++ cho `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Tài liệu tham khảo C++ cho `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Chi tiết về `std::ranges` của C++20: https://en.cppreference.com/w/cpp/ranges

Thèm hiểu biết về Unicode? Thử Dự án ICU:
- Dự án ICU: http://site.icu-project.org/home

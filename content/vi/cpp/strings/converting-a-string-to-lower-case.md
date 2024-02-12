---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/cpp/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:57:55.521084-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chuyển đổi một chuỗi thành chữ thường có nghĩa là biến đổi tất cả các chữ cái in hoa thành các tương đương in thường của chúng. Lập trình viên thực hiện điều này để đảm bảo tính nhất quán trong nhập liệu của người dùng, xử lý dữ liệu và để đơn giản hóa việc so sánh văn bản.

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

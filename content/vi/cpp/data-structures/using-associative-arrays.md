---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:43.102990-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C++, m\u1EA3ng k\u1EBFt h\u1EE3p \u0111\
  \u01B0\u1EE3c hi\u1EC3n th\u1ECB v\u1EDBi c\xE1c ti\xEAu \u0111\u1EC1 `<map>` v\xE0\
  \ `<unordered_map>`. H\xE3y ph\xE2n t\xEDch c\xE1c v\xED d\u1EE5 \u0111\u1EC3 xem\
  \ c\u1EA3 hai ho\u1EA1t \u0111\u1ED9ng nh\u01B0 th\u1EBF\u2026"
lastmod: '2024-03-13T22:44:37.033501-06:00'
model: gpt-4-0125-preview
summary: "Trong C++, m\u1EA3ng k\u1EBFt h\u1EE3p \u0111\u01B0\u1EE3c hi\u1EC3n th\u1ECB\
  \ v\u1EDBi c\xE1c ti\xEAu \u0111\u1EC1 `<map>` v\xE0 `<unordered_map>`."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Trong C++, mảng kết hợp được hiển thị với các tiêu đề `<map>` và `<unordered_map>`. Hãy phân tích các ví dụ để xem cả hai hoạt động như thế nào.

### Sử dụng `std::map`
`std::map` giữ các phần tử được sắp xếp dựa trên khóa. Dưới đây là cách bạn bắt đầu:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Chèn giá trị
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Truy cập giá trị
    std::cout << "Tuổi của Bob: " << ageMap["Bob"] << std::endl;
    
    // Duyệt qua một map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " là " << pair.second << " tuổi." << std::endl;
    }
    
    return 0;
}
```

### Sử dụng `std::unordered_map`
Khi thứ tự không quan trọng, nhưng hiệu suất làm việc thì có, `std::unordered_map` là bạn của bạn, cung cấp độ phức tạp trung bình nhanh hơn cho việc chèn, tìm kiếm và xóa.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Chèn giá trị
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // Truy cập giá trị
    std::cout << "Giá sữa: $" << productPrice["milk"] << std::endl;
    
    // Duyệt qua một unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " có giá $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Tìm hiểu sâu
Mảng kết hợp trong C++, đặc biệt là `std::map` và `std::unordered_map`, không chỉ đơn giản là lưu trữ các phần tử. Chúng cung cấp nền tảng cho việc quản lý dữ liệu phức tạp hơn bằng cách cho phép thực hiện các thao tác như tìm kiếm, chèn và xóa trong thời gian độ phức tạp hiệu quả (logarithmic cho `std::map` và thời gian trung bình hằng số cho `std::unordered_map`). Hiệu quả này đến từ các cấu trúc dữ liệu cơ bản: một cây cân bằng cho `std::map` và một bảng băm cho `std::unordered_map`.

Theo lịch sử, trước khi chúng trở thành một phần của thư viện chuẩn, các lập trình viên sẽ phải tự mình triển khai các phiên bản của riêng họ hoặc sử dụng thư viện của bên thứ ba, dẫn đến sự không nhất quán và tiềm năng kém hiệu quả. Sự bao gồm của maps trong thư viện chuẩn của C++ không chỉ chuẩn hóa việc sử dụng chúng mà còn tối ưu hóa chúng cho hiệu suất trên các trình biên dịch và nền tảng khác nhau.

Mặc dù cả hai đều mạnh mẽ, lựa chọn giữa `std::map` và `std::unordered_map` phụ thuộc vào chi tiết của trường hợp sử dụng cụ thể của bạn. Cần dữ liệu được sắp xếp và không quan tâm đến sự trao đổi hiệu suất nhỏ? Hãy chọn `std::map`. Nếu bạn đang theo đuổi tốc độ và không quan tâm đến thứ tự, `std::unordered_map` có lẽ là lựa chọn tốt hơn của bạn.

Tuy nhiên, điều quan trọng là phải lưu ý rằng khi làm việc với các cấu trúc dữ liệu phức tạp, luôn có sự trao đổi. Trong một số trường hợp cụ thể, các cấu trúc dữ liệu khác hoặc thậm chí là thư viện của bên thứ ba có thể cung cấp hiệu suất hoặc chức năng tốt hơn phù hợp với nhu cầu cụ thể của bạn. Luôn cân nhắc các lựa chọn của bạn dựa trên yêu cầu của dự án.

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:37.205326-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EAB\
  u c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 kh\u1ECF\
  i m\u1ED9t chu\u1ED7i v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 d\u1ECDn d\u1EB9p, \u0111\u1ECBnh d\u1EA1ng\
  \ d\u1EEF\u2026"
lastmod: '2024-03-11T00:14:10.319697-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu c\xF3\
  \ ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 kh\u1ECF\
  i m\u1ED9t chu\u1ED7i v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 d\u1ECDn d\u1EB9p, \u0111\u1ECBnh d\u1EA1ng\
  \ d\u1EEF\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Xóa các ký tự phù hợp với một mẫu có nghĩa là loại bỏ các chuỗi cụ thể khỏi một chuỗi văn bản. Lập trình viên thực hiện điều này để dọn dẹp, định dạng dữ liệu, hoặc để đáp ứng các quy tắc của ứng dụng.

## Làm Thế Nào:
Hãy loại bỏ các ký tự sử dụng `erase` và `remove_if` cùng với biểu thức lambda. Dưới đây là một ví dụ nhanh:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "B4n4n4!";

    // Loại bỏ tất cả các ký tự số
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    
    std::cout << data << std::endl; // Hiển thị: Bnn!
    
    return 0;
}
```
Kết quả mẫu:
```
Bnn!
```

## Tìm Hiểu Sâu
Thuật toán `std::remove_if` từ tiêu đề `<algorithm>` thực ra không co lại chuỗi; nó sắp xếp lại các phần tử và trả về một con trỏ đến điểm kết thúc logic mới. Phương thức `erase` của lớp `std::string` sau đó loại bỏ "phần thừa" từ cuối. Sự kết hợp này ra đời với C++98 và vẫn hiệu quả và phổ biến.

Có phương án khác không? Đối với các mẫu phức tạp, regex (`<regex>`) là công cụ đa năng của bạn. Nhưng, nó quá mức cần thiết cho những công việc đơn giản.

Chi tiết? `std::remove_if` và các thuật toán tương tự dựa vào các trình lặp, mà C++ đã áp dụng từ Thư viện Mẫu Chuẩn (STL) vào giữa những năm 90. Chúng trao quyền cho lập trình tổng quát, đảm bảo mã code cắt và thay đổi của bạn hoạt động trên chuỗi, danh sách, bạn tên nó.

## Xem Thêm
- Tài liệu tham khảo C++ cho `std::remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
- Tài liệu tham khảo C++ cho `std::string::erase`: https://en.cppreference.com/w/cpp/string/basic_string/erase
- Thêm về trình lặp trong C++: https://en.cppreference.com/w/cpp/iterator
- Khi nào sử dụng `std::regex` cho việc phù hợp với mẫu: https://en.cppreference.com/w/cpp/regex

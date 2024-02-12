---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases:
- /vi/cpp/deleting-characters-matching-a-pattern/
date:                  2024-01-28T21:58:37.205326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

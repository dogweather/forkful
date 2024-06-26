---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:47.944017-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong C++ hi\u1EC7n \u0111\u1EA1i, th\u01B0\
  \ vi\u1EC7n `<chrono>` v\xE0 `<iomanip>` l\xE0 ng\u01B0\u1EDDi b\u1EA1n \u0111\u1ED3\
  ng h\xE0nh c\u1EE7a b\u1EA1n cho c\xE1c thao t\xE1c ng\xE0y-gi\u1EDD. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 m\u1ED9t ph\u01B0\u01A1ng\u2026"
lastmod: '2024-03-13T22:44:37.057692-06:00'
model: gpt-4-0125-preview
summary: "Trong C++ hi\u1EC7n \u0111\u1EA1i, th\u01B0 vi\u1EC7n `<chrono>` v\xE0 `<iomanip>`\
  \ l\xE0 ng\u01B0\u1EDDi b\u1EA1n \u0111\u1ED3ng h\xE0nh c\u1EE7a b\u1EA1n cho c\xE1\
  c thao t\xE1c ng\xE0y-gi\u1EDD."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Cách thực hiện:
Trong C++ hiện đại, thư viện `<chrono>` và `<iomanip>` là người bạn đồng hành của bạn cho các thao tác ngày-giờ. Dưới đây là một phương pháp nhanh chóng sử dụng `std::put_time`:

```cpp
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now(); // Lấy thời gian hiện tại
    auto time = std::chrono::system_clock::to_time_t(now); // Chuyển đổi thành time_t
    
    // Chuyển đổi thành cấu trúc tm để định dạng
    std::tm tm = *std::localtime(&time);

    // Luồng chuỗi cho đầu ra
    std::stringstream ss;

    ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S"); // Định dạng: YYYY-MM-DD HH:MM:SS

    std::string date_str = ss.str(); // Chuyển đổi thành chuỗi

    std::cout << date_str << std::endl; // Xuất chuỗi ngày
    return 0;
}
```

Ví dụ về đầu ra (tùy thuộc vào ngày và giờ hiện tại):
```
2023-03-15 14:25:30
```

## Tìm hiểu kỹ lưỡng
Trước khi `<chrono>` được giới thiệu, các lập trình viên C++ thường phải vật lộn với việc xử lý thời gian theo phong cách C qua `<ctime>`. Điều này kém trực quan và dễ mắc lỗi hơn do quản lý bộ nhớ thủ công và những đặc thù phụ thuộc vào nền tảng.

Các phương án thay thế cho `std::put_time` bao gồm việc sử dụng `strftime`, nhưng đó là cách làm mang phong cách C. Các thư viện bên thứ ba như Boost.Date_Time có thể cung cấp nhiều chức năng hơn nhưng có chi phí thêm phụ thuộc.

Một chi tiết thực hiện quan trọng là hiểu các chỉ định định dạng trong `std::put_time`, giống với những gì được sử dụng trong `strftime`. Bạn đang ánh xạ các trình giữ chỗ cho các thành phần ngày hoặc giờ — `%Y` cho năm đầy đủ, `%m` cho tháng, v.v.

## Xem thêm
- [Tài liệu `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [Tài liệu `<iomanip>`](https://en.cppreference.com/w/cpp/header/iomanip)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)

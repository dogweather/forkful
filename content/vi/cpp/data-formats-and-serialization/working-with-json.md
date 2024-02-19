---
aliases:
- /vi/cpp/working-with-json/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:49.252015-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) trong C++\
  \ bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o d\u1EEF li\u1EC7\
  u v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c \u0111\u1ECBnh d\u1EA1ng nh\u01B0 l\xE0\
  \ JSON. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: 2024-02-18 23:08:51.067347
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) trong C++ bao\
  \ g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o d\u1EEF li\u1EC7\
  u v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c \u0111\u1ECBnh d\u1EA1ng nh\u01B0 l\xE0\
  \ JSON. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Làm việc với JSON (JavaScript Object Notation) trong C++ bao gồm việc phân tích cú pháp và tạo dữ liệu văn bản được định dạng như là JSON. Lập trình viên sử dụng JSON để trao đổi dữ liệu dễ dàng giữa các máy chủ và các ứng dụng web của khách hàng, và bởi vì nó dễ đọc và không phụ thuộc vào ngôn ngữ.

## Làm thế nào:

Để làm việc với JSON trong C++, bạn cần sử dụng một thư viện như `nlohmann/json`. Dưới đây là cách bạn có thể phân tích cú pháp và tạo dữ liệu JSON:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Phân tích JSON
    std::string str = R"({"name":"John", "age":30, "city":"New York"})";
    nlohmann::json parsed = nlohmann::json::parse(str);

    // Truy cập các phần tử
    std::cout << "Tên: " << parsed["name"] << std::endl;
    std::cout << "Tuổi: " << parsed["age"] << std::endl;

    // Tạo JSON
    nlohmann::json j;
    j["name"] = "Jane";
    j["age"] = 25;
    j["city"] = "Los Angeles";

    std::cout << "JSON được tạo: " << j.dump(4) << std::endl;

    return 0;
}
```

Kết quả Mẫu:
```
Tên: John
Tuổi: 30
JSON được tạo: {
    "age": 25,
    "city": "Los Angeles",
    "name": "Jane"
}
```

## Sâu hơn:

JSON được giới thiệu như một định dạng văn bản đơn giản cho việc trao đổi dữ liệu và trở thành tiêu chuẩn do tính đơn giản và sự áp dụng rộng rãi của nó. Các lựa chọn khác như XML tồn tại nhưng JSON dẫn đầu trong các API web do có tính lược bớt và dễ đọc hơn. C++ không có hỗ trợ JSON nguyên bản, do đó các thư viện như `nlohmann/json` được ưa chuộng để xử lý việc serial hóa và deserial hóa, cung cấp một API sạch sẽ giống như làm việc với các kiểu dữ liệu nguyên bản.

## Xem Thêm:

- Kho GitHub cho `nlohmann/json`: https://github.com/nlohmann/json
- Trang web chính thức của JSON để biết thêm về định dạng: https://www.json.org/json-en.html
- Đối với xử lý XML trong C++: https://pugixml.org/
- Trang Cppreference về dòng chuỗi để xử lý chuỗi nâng cao trong C++: https://en.cppreference.com/w/cpp/io/basic_stringstream

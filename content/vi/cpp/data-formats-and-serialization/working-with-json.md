---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:49.252015-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON\
  \ trong C++, b\u1EA1n c\u1EA7n s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0\
  \ `nlohmann/json`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3 th\u1EC3\
  \ ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:37.069968-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON trong C++, b\u1EA1n c\u1EA7\
  n s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `nlohmann/json`."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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

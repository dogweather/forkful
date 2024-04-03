---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:07.611555-07:00
description: "G\xEC & T\u1EA1i sao? TOML (Tom's Obvious, Minimal Language - Ng\xF4\
  n ng\u1EEF R\xF5 r\xE0ng, T\u1ED1i gi\u1EA3n c\u1EE7a Tom) l\xE0 \u0111\u1ECBnh\
  \ d\u1EA1ng h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\u1ECDc nh\u1EDD v\xE0o ng\u1EEF\
  \ ngh\u0129a r\xF5 r\xE0ng c\u1EE7a\u2026"
lastmod: '2024-03-13T22:44:37.072129-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - Ng\xF4n ng\u1EEF R\xF5 r\xE0ng,\
  \ T\u1ED1i gi\u1EA3n c\u1EE7a Tom) l\xE0 \u0111\u1ECBnh d\u1EA1ng h\xF3a d\u1EEF\
  \ li\u1EC7u d\u1EC5 \u0111\u1ECDc nh\u1EDD v\xE0o ng\u1EEF ngh\u0129a r\xF5 r\xE0\
  ng c\u1EE7a n\xF3."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

### Gì & Tại sao?
TOML (Tom's Obvious, Minimal Language - Ngôn ngữ Rõ ràng, Tối giản của Tom) là định dạng hóa dữ liệu dễ đọc nhờ vào ngữ nghĩa rõ ràng của nó. Lập trình viên sử dụng TOML cho các tệp cấu hình bởi vì nó cân bằng giữa khả năng đọc của con người và khả năng phân tích của máy.

### Làm thế nào:
Để làm việc với TOML trong C++, bạn sẽ cần một thư viện như `toml++`. Dưới đây là hướng dẫn nhanh:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Phân tích TOML từ một tệp
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Truy cập một giá trị
    std::string title = config["title"].value_or("Không tiêu đề");
    std::cout << "Tiêu đề: " << title << '\n';

    // Chỉnh sửa và lưu TOML
    config["title"] = "Tiêu đề Mới";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Mẫu `config.toml`:
```toml
title = "Ví dụ"
```

Kết quả mẫu:
```plaintext
Tiêu đề: Ví dụ
```

### Sâu hơn
TOML được Tom Preston-Werner tạo ra vào năm 2013 như một lựa chọn thay thế cho YAML và JSON. Nó được thiết kế để đơn giản và rõ ràng, chủ yếu dành cho tệp cấu hình. Khác với JSON, TOML tập trung vào việc không mơ hồ, có nghĩa là nó quyết định cách tài liệu được phân tích.

Các lựa chọn thay thế cho TOML bao gồm YAML, nó linh hoạt hơn trong những gì được cho phép, tuy nhiên đôi khi với giá của tính dự đoán. JSON, một lựa chọn khác, khá khắt khe về cấu trúc nhưng không thân thiện với con người như cấu hình do thiếu bình luận và cú pháp nặng nề về dấu ngoặc.

Trong việc triển khai, `toml++` là một thư viện chỉ dùng header C++17 tuân thủ với đặc tả TOML mới nhất. Nó cung cấp một giao diện giống như DOM để điều hướng và thao tác dữ liệu TOML, làm cho việc tích hợp vào các dự án trở nên trực tiếp. Thư viện này đảm nhận việc phân tích, xác thực và tạo ra đầu ra, cho phép bạn lấy và đặt dữ liệu TOML sử dụng các kiểu C++.

### Xem thêm
- Kho lưu trữ GitHub của TOML: https://github.com/toml-lang/toml
- `toml++`, một thư viện C++ cho TOML: https://github.com/marzer/tomlplusplus
- Tài liệu chính thức của TOML với những giải thích chi tiết về định dạng: https://toml.io/en/

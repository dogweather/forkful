---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:34.837298-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ng\xE0y x\u01B0a, vi\u1EC7c l\u01B0u tr\u1EEF\
  \ v\xE0 truy xu\u1EA5t d\u1EEF li\u1EC7u kh\xE1 g\u1EB7p kh\xF3 kh\u0103n. V\u1EDB\
  i s\u1EF1 ra \u0111\u1EDDi c\u1EE7a c\xE1c ng\xF4n ng\u1EEF l\u1EADp tr\xECnh c\u1EA5\
  p cao, c\xE1c ho\u1EA1t \u0111\u1ED9ng nh\u01B0 \u0111\u1ECDc t\u1EEB\u2026"
lastmod: '2024-04-05T22:50:51.373937-06:00'
model: gpt-4-0125-preview
summary: "Ng\xE0y x\u01B0a, vi\u1EC7c l\u01B0u tr\u1EEF v\xE0 truy xu\u1EA5t d\u1EEF\
  \ li\u1EC7u kh\xE1 g\u1EB7p kh\xF3 kh\u0103n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thực hiện:
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt");
    std::string line;

    if (file.is_open()) {
        while (getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Không thể mở tệp";
    }
    
    return 0;
}
```
Nếu `example.txt` chứa:
```
Chào thế giới!
Đây là một tệp thử nghiệm.
```
Kết quả sẽ là:
```
Chào thế giới!
Đây là một tệp thử nghiệm.
```

## Sâu hơn nữa
Ngày xưa, việc lưu trữ và truy xuất dữ liệu khá gặp khó khăn. Với sự ra đời của các ngôn ngữ lập trình cấp cao, các hoạt động như đọc từ một tệp văn bản trở nên đơn giản hơn. C++ cung cấp một vài cách để đọc từ các tệp, tận dụng các luồng vào/ra của thư viện tiêu chuẩn.

Các phương thức thay thế cho <fstream> cho việc I/O tệp bao gồm sử dụng các hàm C cũ (như fopen, fgets, v.v.), API cụ thể của hệ điều hành, hoặc các thư viện khác tóm tắt bớt một số chi tiết cấp thấp.

Khi chúng ta nói về chi tiết triển khai, điều cần thiết là biết rằng `std::ifstream` là một lớp xử lý luồng tệp nhập vào. Các chức năng chính liên quan là `is_open()` để kiểm tra xem luồng tệp đã được mở thành công hay không, `getline()` để đọc tệp dòng trên dòng, và `close()` để đóng luồng tệp. Việc quản lý tài nguyên tệp một cách chính xác là rất quan trọng để tránh rò rỉ hoặc hỏng dữ liệu. May mắn thay, C++ hiện đại (C++11 trở lên) bao gồm các tính năng như RAII, có thể xử lý quản lý tài nguyên một cách an toàn hơn thông qua thời gian sống của đối tượng.

## Xem thêm
- [cppreference.com - Thư viện vào/ra](https://en.cppreference.com/w/cpp/io)
- Stack Overflow: [Làm thế nào để đọc và phân tích các tệp CSV trong C++?](https://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c)

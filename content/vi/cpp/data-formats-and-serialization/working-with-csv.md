---
title:                "Làm việc với CSV"
aliases: - /vi/cpp/working-with-csv.md
date:                  2024-01-28T22:10:16.965826-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Làm việc với CSV (Comma-Separated Values hay giá trị được phân tách bằng dấu phẩy) có nghĩa là xử lý các tệp văn bản đơn giản lưu trữ dữ liệu dạng bảng. Lập trình viên sử dụng CSV vì nó đơn giản và tương thích trên nhiều hệ thống, hoàn hảo cho việc trao đổi dữ liệu giữa các phần mềm khác nhau.

## Làm thế nào:

Dưới đây là một đoạn mã đọc tệp CSV và in nội dung của nó.

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

int main() {
    std::string line, cell;
    std::vector<std::vector<std::string>> csvData;
    std::ifstream file("example.csv");

    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::vector<std::string> rowData;
        
        while (std::getline(lineStream, cell, ',')) {
            rowData.push_back(cell);
        }
        csvData.push_back(rowData);
    }
    
    for (const auto& row : csvData) {
        for (const auto& col : row) {
            std::cout << col << " ";  // Tùy vào cấu trúc CSV của bạn, hãy điều chỉnh dấu phân cách.
        }
        std::cout << std::endl;
    }
    return 0;
}
```

Kết quả mẫu cho một CSV chứa tên và tuổi:
```
John 25
Jane 28
```

## Đi sâu hơn

CSV đã tồn tại từ đầu những năm 1970. Đây là định dạng đi đến cho việc xuất khẩu và nhập dữ liệu đơn giản nhưng không tốt cho dữ liệu phân cấp phức tạp, mà XML và JSON xử lý tốt hơn. C++ không hỗ trợ CSV một cách sẵn có, nhưng việc xử lý tệp và chuỗi là khá trực tiếp. Bạn giải quyết với I/O chuẩn và thao tác chuỗi, trong khi chú ý đến các trường hợp ngoại lệ như dấu ngoặc kép và dấu phẩy trong các ô. Các thư viện như `libcsv` và `Boost.Tokenizer` có thể đơn giản hóa nhiệm vụ nếu bạn đang xử lý các tệp CSV phức tạp hơn.

## Xem thêm

- [RFC 4180](https://tools.ietf.org/html/rfc4180), định dạng chung và loại MIME cho các tệp CSV.
- [Tham khảo C++ cho I/O](http://www.cplusplus.com/reference/fstream/)
- [Thư viện C++ Boost](https://www.boost.org/)
- [10 phút với pandas - Xử lý CSV bằng Python để so sánh](https://pandas.pydata.org/pandas-docs/stable/user_guide/10min.html)

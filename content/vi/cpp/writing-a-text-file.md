---
title:                "Viết một tệp văn bản"
aliases:
- vi/cpp/writing-a-text-file.md
date:                  2024-01-28T22:12:18.722389-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì và Tại sao?
Viết một tệp văn bản trong C++ có nghĩa là tạo hoặc chỉnh sửa một tệp để lưu trữ dữ liệu văn bản. Lập trình viên thực hiện để lưu trữ dữ liệu như cấu hình, nhật ký, hoặc nội dung do người dùng tạo.

## Cách thực hiện:
Dưới đây là một chương trình C++ đơn giản tạo một tệp văn bản và viết "Hello, World!" vào đó.

```c++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("hello.txt");

    if (outfile.is_open()) {
        outfile << "Hello, World!";
        outfile.close();
        std::cout << "Ghi tệp thành công\n";
    } else {
        std::cout << "Lỗi khi mở tệp\n";
    }

    return 0;
}
```
Kết quả mẫu:
```
Ghi tệp thành công
```

## Đi sâu hơn
Trong C++, tệp được xử lý bởi tiêu đề `<fstream>`, cung cấp `std::ofstream` để ghi, `std::ifstream` để đọc, và `std::fstream` để thực hiện cả hai. Lịch sử, nhập/xuất tệp trong C++ đã phát triển từ cấu trúc `FILE` của C và các hàm liên quan. Các phương án thay thế cho `fstream` bao gồm API đặc thù của nền tảng, thư viện bên thứ ba, hoặc đề xuất C++ hiện đại như cải tiến thư viện hệ thống tệp. Khi ghi tệp, xử lý lỗi và đảm bảo nguồn lực được giải phóng đúng cách, thường sử dụng các mẫu RAII có sẵn trong C++ hiện đại.

## Xem thêm
- Nhập/Xuất tệp C++: http://www.cplusplus.com/doc/tutorial/files/
- Tham khảo C++ (ofstream): https://en.cppreference.com/w/cpp/io/basic_ofstream
- Thư viện Hệ thống Tệp C++: https://en.cppreference.com/w/cpp/filesystem

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:26.158045-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch \u0111\u1EC3 t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDD\
  i trong C++ hi\u1EC7n t\u1EA1i."
lastmod: '2024-03-13T22:44:37.067529-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 t\u1EA1o v\xE0 s\u1EED\
  \ d\u1EE5ng m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi trong C++ hi\u1EC7n t\u1EA1i."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Cách thực hiện:
Dưới đây là cách để tạo và sử dụng một tệp tạm thời trong C++ hiện tại:

```C++
#include <cstdio>
#include <filesystem>
#include <iostream>

int main() {
    // Tạo một tệp tạm thời duy nhất bằng cách sử dụng thư viện filesystem
    std::filesystem::path temp_path = std::filesystem::temp_directory_path() /= std::tmpnam(nullptr);

    // Mở tệp tạm thời
    std::FILE* temp_file = std::fopen(temp_path.c_str(), "w+");
    if (!temp_file) {
        std::perror("Mở tệp không thành công");
        return EXIT_FAILURE;
    }

    // Viết một cái gì đó vào đó
    std::fputs("Hello, Temp World!\n", temp_file);

    // Luôn nhớ đóng tệp
    std::fclose(temp_file);

    // Xuất đường dẫn tới tệp tạm thời của chúng tôi
    std::cout << "Tệp tạm thời đã được tạo tại: " << temp_path << std::endl;

    // Dọn dẹp: xóa tệp tạm thời
    std::filesystem::remove(temp_path);

    return EXIT_SUCCESS;
}
```

Đầu ra mẫu (đường dẫn thực tế sẽ thay đổi):

```
Tệp tạm thời đã được tạo tại: /tmp/abc123
```

## Sâu hơn nữa
Tệp tạm thời rất hữu ích trong các trường hợp như lưu trạng thái, sắp xếp các tập dữ liệu lớn, hoặc xử lý đầu ra không cần duy trì lâu dài. Trong lịch sử, tệp tạm thời được tạo trong một thư mục chung (như `/tmp` trên hệ thống Unix) với một cách đặt tên đơn giản, gây nguy cơ va chạm. C++ hiện đại sử dụng thư viện `<filesystem>` để tránh các vấn đề này.

Các phương án thay thế bao gồm sử dụng bộ nhớ tạm thời dựa trên RAM (như tmpfs trong hầu hết các hệ thống giống Unix) hoặc blob cơ sở dữ liệu. Những phương pháp này giữ dữ liệu nhất thời trong bộ nhớ hoặc hệ thống quản lý, giảm tải I/O và cải thiện hiệu suất.

Về mặt triển khai, hãy nhớ rằng:
- I/O tệp có thể thất bại, vì vậy luôn kiểm tra lỗi cho các thao tác tệp của bạn.
- Luôn đóng tệp của bạn để ngăn chặn rò rỉ tài nguyên.
- Dọn dẹp: Xóa tệp tạm thời của bạn (mặc dù hệ thống thường làm điều này, nhưng là một thói quen tốt).

## Xem Thêm
- [Thư Viện Filesystem C++](https://en.cppreference.com/w/cpp/filesystem)
- [Thư Viện IOstreams C++](https://en.cppreference.com/w/cpp/io)
- [Xử Lý Tệp Tạm Thời trong C](http://www.cplusplus.com/reference/cstdio/tmpfile/)

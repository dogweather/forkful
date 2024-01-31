---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:26.158045-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Tạo một tệp tạm thời có nghĩa là tạo ra một tệp được thiết kế để lưu trữ dữ liệu một cách tạm thời và sẽ bị xóa sau khi sử dụng. Lập trình viên làm điều này để xử lý dữ liệu trung gian mà không làm rối loạn hệ thống tệp hay rủi ro xung đột với các tệp khác.

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

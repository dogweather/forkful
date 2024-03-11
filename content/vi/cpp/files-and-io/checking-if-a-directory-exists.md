---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:27.389571-07:00
description: '#'
lastmod: '2024-03-11T00:14:10.361795-06:00'
model: gpt-4-0125-preview
summary: '#'
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Kiểm tra sự tồn tại của thư mục trong C++

### Là gì & Tại sao?

Kiểm tra sự tồn tại của thư mục là để xác nhận xem thư mục chỉ định có hiện diện trên hệ thống tệp hay không. Lập trình viên làm điều này để tránh lỗi khi truy cập, đọc hoặc ghi vào tệp - giống như việc đảm bảo rằng ngăn kéo thực sự ở đó trước khi bạn cất quần áo vào.

### Cách thực hiện:

Bắt đầu với C++17, chúng ta có `std::filesystem` để làm cho cuộc sống của mình dễ dàng hơn trong các thao tác hệ thống tệp. Dưới đây là đoạn mã để kiểm tra nếu một thư mục tồn tại:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dir_path{"./some_directory"};

    bool exists = std::filesystem::exists(dir_path);
    if(exists) {
        std::cout << "Directory exists." << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;
    }

    return 0;
}
```

Đầu ra mẫu (nếu thư mục tồn tại):
```
Directory exists.
```

Hoặc (nếu thư mục không tồn tại):
```
Directory does not exist.
```

### Sâu hơn một chút

Trước C++17, chúng ta phải dựa vào các lời gọi API cụ thể của hệ thống hoặc thư viện bên thứ ba. Trong API Windows, chúng ta có thể đã sử dụng `GetFileAttributes` và kiểm tra xem giá trị trả về có phải là `INVALID_FILE_ATTRIBUTES` hay không. Trên các hệ thống POSIX, chúng ta có thể sử dụng hàm `stat()` để có chức năng tương tự.

C++17 đã thay đổi cuộc chơi với `std::filesystem`. Nó cung cấp hỗ trợ đa nền tảng và giao diện cấp cao để tương tác với hệ thống tệp. Hàm `exists()` là cách trực tiếp để kiểm tra sự tồn tại của thư mục, nhưng bạn cũng có thể sử dụng `is_directory()` nếu bạn không chỉ muốn xác nhận sự tồn tại mà còn muốn xem đường dẫn có chỉ đến một thư mục chứ không phải là một tệp hay không.

Đối với các phương pháp khác, hãy xem xét `std::filesystem::status_known()` và `std::filesystem::file_status` để xử lý các trường hợp mà quyền truy cập vào tệp hoặc các vấn đề khác có thể ảnh hưởng đến khả năng của bạn để xác định sự tồn tại của một thư mục.

### Xem Thêm

Khám phá thêm về các thao tác hệ thống tệp trong C++:

- [Tài liệu std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- Để hiểu thêm về bối cảnh lịch sử và sự khác biệt giữa các phiên bản, xem [Lịch sử phiên bản C++](https://en.cppreference.com/w/cpp/compiler_support)

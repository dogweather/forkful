---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:55.249656-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Các đối số dòng lệnh cho phép người dùng ảnh hưởng đến hành vi của một chương trình mà không cần thay đổi mã. Các chương trình sử dụng chúng để nhận các tham số đầu vào, đường dẫn tệp, hoặc chế độ hoạt động, giúp tiết kiệm thời gian và tăng tính linh hoạt.

## Làm thế nào:
Trong C++, các đối số dòng lệnh được nhận trong `main()` như một mảng các con trỏ ký tự. Dưới đây là cách bạn có thể lấy chúng:

```C++
#include <iostream>
int main(int argc, char* argv[]) {
    std::cout << "Bạn đã nhập " << argc << " đối số:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << argv[i] << "\n";
    }
    return 0;
}
```

Kết quả Mẫu: (Giả sử thực thi như `./myProgram foo bar`)

```plaintext
Bạn đã nhập 3 đối số:
./myProgram
foo
bar
```

## Sâu hơn
Ngày xửa ngày xưa, dòng lệnh là cách duy nhất để tương tác với các chương trình. Giao diện người dùng đồ họa (GUI) ngày nay rất tuyệt, nhưng dòng lệnh vẫn tồn tại, đặc biệt là trong môi trường máy chủ hoặc phát triển. Nó cung cấp kiểm soát nhanh chóng, có thể tự động hóa qua script.

Các phương án thay thế cho `argv` và `argc` có sẵn bao gồm các thư viện như `Boost.Program_options` cho việc phân tích cú pháp tinh tế hơn. Cũng có hàm `getopt()` trong các hệ thống giống Unix cho những người hâm mộ dòng lệnh truyền thống.

Thực hiện phân tích đối số từ đầu cho phép bạn tùy chỉnh nó, nhưng hãy chú ý đến các lỗ hổng bảo mật. Đừng mù quáng tin tưởng vào đầu vào từ người dùng - luôn luôn xác thực và làm sạch dữ liệu.

## Xem thêm
- Tài liệu C++ về hàm `main()`: https://en.cppreference.com/w/cpp/language/main_function
- Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- Hướng dẫn `getopt()` của GNU: https://www.gnu.org/software/libc/manual/html_node/Getopt.html

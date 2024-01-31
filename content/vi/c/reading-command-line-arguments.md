---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:25.970265-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Có lẽ, mọi lập trình viên đều đã từng gặp phải đối số dòng lệnh. Chúng cho phép người dùng cung cấp dữ liệu cho chương trình của bạn. Điều chỉnh chúng có thể thay đổi cách chương trình của bạn hoạt động một cách đáng kể mà không cần thay đổi code—hãy nghĩ đến mã gian lận dành cho game thủ, nhưng dành cho lập trình viên.

## Cách thực hiện:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Bạn đã nhập %d đối số:\n", argc);
    for(int i = 0; i < argc; i++) {
        printf("arg%d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Output mẫu khi gọi `./yourprogram Hello World!`:

```
Bạn đã nhập 3 đối số:
arg0: ./yourprogram
arg1: Hello
arg2: World!
```

## Đi Sâu

Ngày xưa trong kỷ nguyên Unix, dòng lệnh terminal là sự lựa chọn hàng đầu. Ngày nay, Giao diện người dùng đồ họa (GUIs) là vua, nhưng đối số dòng lệnh chưa hề lỗi thời—hãy nghĩ đến scripts, công việc tự động, hoặc tham số ứng dụng phức tạp.

Đối số trong C được truyền qua hai tham số trong `main()`: `argc` (số lượng đối số) và `argv` (vector đối số). `argc` cho bạn biết có bao nhiêu đối số, trong khi `argv` là một mảng các chuỗi chứa các đối số thực tế, `argv[0]` là tên của chương trình.

Có những phương án thay thế như `getopt()` dành cho các hệ thống giống Unix, có thể phân tích các tùy chọn và đối số của chúng một cách đẹp đẽ. Ngoài ra, các thư viện như `argp` giúp bạn trong các tình huống phân tích phức tạp hơn.

Việc hiểu biết chi tiết liên quan đến con trỏ và mảng, vì `argv` là một mảng các con trỏ ký tự—cơ bản là một loạt các chuỗi. Khi xử lý dòng lệnh, chương trình của bạn coi khoảng trắng là bộ phân tách đối số, trừ khi dấu ngoặc kép là bạn của bạn, bao quanh một đối số duy nhất.

## Xem Thêm

- Mục về [Đối số Chương trình](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html) trong Hướng dẫn Thư viện C của GNU
- [Bài viết về Giao diện dòng lệnh](https://en.wikipedia.org/wiki/Command-line_interface#Arguments) trên Wikipedia
- ["Đối số Dòng Lệnh C" trên tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)

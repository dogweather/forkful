---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:39.888928-07:00
description: "L\xE0m c\xE1ch n\xE0o: H\xE3y xem x\xE9t m\u1ED9t \u0111o\u1EA1n script\
  \ Bash \u0111\u01A1n gi\u1EA3n c\u1EA7n \u0111\u01B0\u1EE3c refactoring. N\xF3 c\u1ED3\
  ng k\u1EC1nh, v\u1EDBi m\xE3 l\u1EB7p l\u1EA1i v\xE0 kh\xF3 theo d\xF5i."
lastmod: '2024-03-13T22:44:36.890220-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y xem x\xE9t m\u1ED9t \u0111o\u1EA1n script Bash \u0111\u01A1n gi\u1EA3\
  n c\u1EA7n \u0111\u01B0\u1EE3c refactoring."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm cách nào:
Hãy xem xét một đoạn script Bash đơn giản cần được refactoring. Nó cồng kềnh, với mã lặp lại và khó theo dõi:
```Bash
#!/bin/bash
echo "Nhập tên tệp:"
read filename
if [ -f "$filename" ]; then
    echo "Tệp tồn tại."
    count=$(grep -c "foo" "$filename")
    echo "Từ foo xuất hiện $count lần."
else
    echo "Tệp không tồn tại."
fi
```
Refactoring để tăng sự rõ ràng và khả năng tái sử dụng có thể bao gồm việc giới thiệu các chức năng và xử lý lỗi một cách duyên dáng hơn:
```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Nhập tên tệp:"
    read -r filename
    echo "Nhập từ bạn muốn tìm:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Từ $word xuất hiện $count lần."
    else
        echo "Tệp không tồn tại." >&2
        exit 1
    fi
}

main "$@"
```
Phiên bản đã được refactoring sử dụng các hàm để cải thiện khả năng đọc và cho phép khả năng sử dụng lại tiềm năng.

## Sâu hơn:
Refactoring không phải là một khái niệm bắt đầu với Bash hay thậm chí là ngôn ngữ lập trình cấp cao; nó cũ như chính việc lập trình. Thuật ngữ này được chính thức hóa trong cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler vào năm 1999, tập trung chủ yếu vào ngôn ngữ hướng đối tượng.

Trong bối cảnh của kịch bản Bash, refactoring thường có nghĩa là phân chia các kịch bản dài thành các hàm, giảm bớt sự lặp lại với các vòng lặp hoặc điều kiện, và tránh các sai lầm phổ biến như không xử lý được khoảng trắng trong tên tệp. Cách thay thế cho Bash đối với các kịch bản đã trở nên quá phức tạp bao gồm Python hoặc Perl, chúng cung cấp cấu trúc dữ liệu tốt hơn và xử lý lỗi cho các nhiệm vụ phức tạp.

Refactoring cụ thể cho Bash nhiều hơn là tuân theo các phương pháp tốt nhất, như sử dụng dấu ngoặc cho biến, sử dụng `[[ ]]` cho các bài kiểm tra thay vì `[ ]`, và ưu tiên `printf` hơn là `echo` cho đầu ra mạnh mẽ. Chi tiết triển khai thường xoay quanh việc tuân theo hướng dẫn phong cách và sử dụng công cụ như `shellcheck` cho phân tích tĩnh để bắt các lỗi phổ biến.

## Xem thêm:
- [Hướng dẫn Phong cách Shell của Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, một công cụ phân tích tĩnh cho kịch bản shell](https://www.shellcheck.net/)
- [Nghệ thuật Dòng Lệnh](https://github.com/jlevy/the-art-of-command-line)

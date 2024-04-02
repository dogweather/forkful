---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:45.057558-07:00
description: "Bash kh\xF4ng c\xF3 tr\xECnh g\u1EE1 l\u1ED7i t\xEDch h\u1EE3p s\u1EB5\
  n gi\u1ED1ng nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c, nh\u01B0ng b\u1EA1\
  n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1c l\u1EC7nh t\xEDch h\u1EE3p nh\u01B0 `set\
  \ -x` \u0111\u1EC3 theo d\xF5i nh\u1EEFng g\xEC \u0111ang\u2026"
lastmod: '2024-03-13T22:44:36.885093-06:00'
model: gpt-4-0125-preview
summary: "Bash kh\xF4ng c\xF3 tr\xECnh g\u1EE1 l\u1ED7i t\xEDch h\u1EE3p s\u1EB5n\
  \ gi\u1ED1ng nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c, nh\u01B0ng b\u1EA1\
  n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1c l\u1EC7nh t\xEDch h\u1EE3p nh\u01B0 `set\
  \ -x` \u0111\u1EC3 theo d\xF5i nh\u1EEFng g\xEC \u0111ang\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Cách thực hiện:
Bash không có trình gỡ lỗi tích hợp sẵn giống như một số ngôn ngữ khác, nhưng bạn có thể sử dụng các lệnh tích hợp như `set -x` để theo dõi những gì đang xảy ra. Hoặc, để nâng cấp, có `bashdb`, một trình gỡ lỗi thích hợp để bước qua mã của bạn. Đây là một cái nhìn sơ qua:

```Bash
# Sử dụng set -x để gỡ lỗi
set -x
echo "Bắt đầu gỡ lỗi"
my_var="Hello, Thế giới Debug!"
echo $my_var
set +x

# Sử dụng bashdb
# Cài đặt bashdb với trình quản lý gói của bạn, ví dụ: apt, yum, brew.
# Gỡ lỗi một script gọi là my_script.sh:
bashdb my_script.sh
```

Kết quả khi chạy với `set -x`:
```Bash
+ echo 'Bắt đầu gỡ lỗi'
Bắt đầu gỡ lỗi
+ my_var='Hello, Thế giới Debug!'
+ echo 'Hello, Thế giới Debug!'
Hello, Thế giới Debug!
+ set +x
```

## Sâu hơn nữa
Trước đây, gỡ lỗi các script Bash có nghĩa là làm cho mã của bạn chứa đầy các lệnh `echo`. Nhưng sau đó, `set -x` xuất hiện, cho chúng ta cái nhìn vào việc thực thi thời gian chạy mà không cần in thủ công. Và cho những ai đang khao khát kiểm soát nhiều hơn, trình gỡ lỗi `bashdb` xuất hiện, lấy cảm hứng từ trình gỡ lỗi gdb cho C/C++.

Về các phương án thay thế, ngoài các lệnh `set` (`-x`, `-v`, `-e`), các lựa chọn khác bao gồm chuyển hướng đầu ra sang một tệp để phân tích hoặc sử dụng các công cụ bên ngoài như ShellCheck cho phân tích tĩnh.

Về mặt triển khai, `set -x` dễ dàng; đó là một tùy chọn Bash tự nhiên in ra các lệnh và các đối số của chúng khi chúng được thực thi. `bashdb`, mặt khác, cho phép điều hướng qua mã, đặt các điểm dừng, và đánh giá các biểu thức - những thứ cho bạn cơ hội chiến đấu chống lại các lỗi khó chịu hơn.

## Xem thêm
- Dự án Bash Debugger: http://bashdb.sourceforge.net/
- "Pro Bash Programming" của Chris Johnson và Jayant Varma cho lập trình nâng cao.
- ShellCheck cho phân tích tĩnh: https://www.shellcheck.net/

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:45.057558-07:00
description: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i trong Bash c\xF3\
  \ ngh\u0129a l\xE0 s\u1EED d\u1EE5ng c\xE1c c\xF4ng c\u1EE5 \u0111\u1EC3 ki\u1EC3\
  m tra v\xE0 t\xECm l\u1ED7i trong c\xE1c script c\u1EE7a b\u1EA1n, ch\u1EB3ng h\u1EA1\
  n nh\u01B0 b\u1EAFt l\u1ED7i l\xE0m s\u1EADp m\xE3 c\u1EE7a\u2026"
lastmod: '2024-02-25T18:49:35.230218-07:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i trong Bash c\xF3 ngh\u0129\
  a l\xE0 s\u1EED d\u1EE5ng c\xE1c c\xF4ng c\u1EE5 \u0111\u1EC3 ki\u1EC3m tra v\xE0\
  \ t\xECm l\u1ED7i trong c\xE1c script c\u1EE7a b\u1EA1n, ch\u1EB3ng h\u1EA1n nh\u01B0\
  \ b\u1EAFt l\u1ED7i l\xE0m s\u1EADp m\xE3 c\u1EE7a\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Sử dụng một trình gỡ lỗi trong Bash có nghĩa là sử dụng các công cụ để kiểm tra và tìm lỗi trong các script của bạn, chẳng hạn như bắt lỗi làm sập mã của bạn hoặc khiến nó hoạt động không đúng cách một cách lén lút. Các lập trình viên làm điều này vì nó thông minh hơn nhiều khi bắt lỗi trước khi chúng gây ra hậu quả trong môi trường trực tiếp.

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

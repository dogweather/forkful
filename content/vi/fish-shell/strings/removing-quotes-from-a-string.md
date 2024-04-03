---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:16.428456-07:00
description: "X\xF3a d\u1EA5u ngo\u1EB7c t\u1EEB m\u1ED9t chu\u1ED7i l\xE0 vi\u1EC7\
  c lo\u1EA1i b\u1ECF nh\u1EEFng d\u1EA5u ngo\u1EB7c k\xE9p (\" \") ho\u1EB7c d\u1EA5\
  u ngo\u1EB7c \u0111\u01A1n (' ') kh\xF3 ch\u1ECBu kh\u1ECFi d\u1EEF li\u1EC7u v\u0103\
  n b\u1EA3n c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng\u2026"
lastmod: '2024-03-13T22:44:37.192236-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a d\u1EA5u ngo\u1EB7c t\u1EEB m\u1ED9t chu\u1ED7i l\xE0 vi\u1EC7c lo\u1EA1\
  i b\u1ECF nh\u1EEFng d\u1EA5u ngo\u1EB7c k\xE9p (\" \") ho\u1EB7c d\u1EA5u ngo\u1EB7\
  c \u0111\u01A1n (' ') kh\xF3 ch\u1ECBu kh\u1ECFi d\u1EEF li\u1EC7u v\u0103n b\u1EA3\
  n c\u1EE7a b\u1EA1n."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Làm thế nào:
Fish có phép màu tích hợp cho loại nhiệm vụ này. Sử dụng hàm `string` mà không cần mò mẫm. Hãy thử xem những phép thuật này:

```fish
# Ví dụ với dấu ngoặc đơn
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Kết quả: Hello, World!

# Trường hợp tương tự với dấu ngoặc kép
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Kết quả: Hello, Universe!
```

## Sâu hơn
Trở lại thời đại đá của dòng lệnh, bạn sẽ đấu tranh với `sed` hoặc `awk` để xóa dấu ngoặc; một mớ bòng bong của dấu gạch chéo ngược và cờ bí ẩn. Hàm `string` của Fish đến từ một kỷ nguyên mới, làm cho mã sạch sẽ và trực quan hơn.

Các biện pháp thay thế trong các shell khác có thể vẫn dựa vào những công cụ cũ này hoặc có thể sử dụng phương pháp tích hợp riêng của họ như việc mở rộng tham số của bash hoặc bộ điều chỉnh của zsh.

Hàm `string` đi xa hơn việc cắt bỏ dấu ngoặc. Đó là một công cụ đa năng cho các thao tác chuỗi trong Fish. Với `string`, bạn có thể cắt, chia, tách, kết hợp, hoặc thậm chí là khớp regex với chuỗi ngay trong terminal của mình.

## Xem thêm
Khám phá sâu hơn về `string` với sự giúp đỡ của tài liệu chính thức:
- [Tài liệu Chuỗi của Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Đối với sự hoài niệm hoặc khi viết kịch bản với các shell truyền thống hơn, hãy xem:
- [Hướng dẫn Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Mở rộng Tham số Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)

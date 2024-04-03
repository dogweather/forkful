---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:16.428456-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish c\xF3 ph\xE9p m\xE0u t\xEDch h\u1EE3p cho\
  \ lo\u1EA1i nhi\u1EC7m v\u1EE5 n\xE0y. S\u1EED d\u1EE5ng h\xE0m `string` m\xE0 kh\xF4\
  ng c\u1EA7n m\xF2 m\u1EABm. H\xE3y th\u1EED xem nh\u1EEFng ph\xE9p thu\u1EADt n\xE0\
  y."
lastmod: '2024-03-13T22:44:37.192236-06:00'
model: gpt-4-0125-preview
summary: "Fish c\xF3 ph\xE9p m\xE0u t\xEDch h\u1EE3p cho lo\u1EA1i nhi\u1EC7m v\u1EE5\
  \ n\xE0y."
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

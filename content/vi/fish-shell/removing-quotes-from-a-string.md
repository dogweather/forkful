---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:16.428456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Xóa dấu ngoặc từ một chuỗi là việc loại bỏ những dấu ngoặc kép (" ") hoặc dấu ngoặc đơn (' ') khó chịu khỏi dữ liệu văn bản của bạn. Lập trình viên thường làm điều này để làm sạch dữ liệu đầu vào hoặc chuẩn bị dữ liệu cho quá trình xử lý tiếp theo mà không bị lộn xộn bởi dấu ngoặc.

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

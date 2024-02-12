---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- /vi/fish-shell/finding-the-length-of-a-string/
date:                  2024-01-28T22:00:11.317768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Tìm độ dài của một chuỗi nghĩa là đếm số lượng ký tự trong đó. Các lập trình viên thực hiện điều này để xác thực đầu vào, kích cỡ buffer, hoặc lặp qua các ký tự.

## Cách thực hiện:
Dưới đây là cách để lấy độ dài của một chuỗi trong Fish:

```Fish Shell
set my_string "Hello, World!"
echo (string length "$my_string")
```

Kết quả:

```
13
```

## Sâu hơn
Trong Fish, không giống như một số shells khác, `string length` là một hàm được xây dựng sẵn làm cho nó trở nên tự nhiên và hiệu quả. Trong lịch sử, các shell khác có thể yêu cầu cú pháp dài dòng hơn hoặc công cụ bên ngoài như `expr` hoặc `wc`. Fish làm đơn giản hóa các tác vụ với các hàm xử lý chuỗi mạnh mẽ của mình, nơi mà `string length` trực tiếp cung cấp số lượng ký tự Unicode, điều này không luôn bằng với số byte, đặc biệt là đối với các ký tự không phải ASCII.

Các phương án thay thế để xác định độ dài chuỗi trong các shell trước hàm `string` trong Fish có thể kém đáng tin cậy hơn bởi vì chúng không luôn tính đến các ký tự đa byte. Về mặt triển khai, `string length` đếm các grapheme Unicode, điều này quan trọng đối với các văn bản chứa các ký tự kết hợp với nhau để tạo thành một đơn vị trực quan duy nhất.

## Xem Thêm
- Tài liệu Fish về thao tác chuỗi: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Tiêu chuẩn Unicode để hiểu về graphemes: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)

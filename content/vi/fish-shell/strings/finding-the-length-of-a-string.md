---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:11.317768-07:00
description: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 trong \u0111\xF3. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1\
  c th\u1EF1c \u0111\u1EA7u v\xE0o, k\xEDch c\u1EE1 buffer, ho\u1EB7c l\u1EB7p qua\
  \ c\xE1c\u2026"
lastmod: '2024-03-13T22:44:37.196042-06:00'
model: gpt-4-0125-preview
summary: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 trong \u0111\xF3. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1\
  c th\u1EF1c \u0111\u1EA7u v\xE0o, k\xEDch c\u1EE1 buffer, ho\u1EB7c l\u1EB7p qua\
  \ c\xE1c\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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

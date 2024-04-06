---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:17.580075-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Fish Shell, \u0111i\u1EC1u k\u1EF3 di\u1EC7\
  u x\u1EA3y ra v\u1EDBi ti\u1EC7n \xEDch `string`, m\u1ED9t c\xF4ng c\u1EE5 ti\u1EC7\
  n l\u1EE3i c\xF3 s\u1EB5n cho c\xE1c thao t\xE1c chu\u1ED7i - \u0111\u01B0\u1EE3\
  c gi\u1EDBi thi\u1EC7u trong phi\xEAn\u2026"
lastmod: '2024-04-05T21:53:38.530532-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish Shell, \u0111i\u1EC1u k\u1EF3 di\u1EC7u x\u1EA3y ra v\u1EDBi\
  \ ti\u1EC7n \xEDch `string`, m\u1ED9t c\xF4ng c\u1EE5 ti\u1EC7n l\u1EE3i c\xF3 s\u1EB5\
  n cho c\xE1c thao t\xE1c chu\u1ED7i - \u0111\u01B0\u1EE3c gi\u1EDBi thi\u1EC7u trong\
  \ phi\xEAn b\u1EA3n 2.3.0."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Làm thế nào:
```Fish Shell
# Xóa số từ một chuỗi
set string "Fish123Shell"
echo $string | string replace -ra '[0-9]' ''
# Kết quả: FishShell

# Loại bỏ tất cả trừ các chữ cái thường
set noisy_string "F!i@s#h$%S^h&e*l(l)__+"
echo $noisy_string | string match -r '[a-z]+'
# Kết quả: ishhell
```

## Đi sâu hơn
Trong Fish Shell, điều kỳ diệu xảy ra với tiện ích `string`, một công cụ tiện lợi có sẵn cho các thao tác chuỗi - được giới thiệu trong phiên bản 2.3.0. Trước đây, người dùng sẽ phải dùng đến các công cụ UNIX quen thuộc như `sed` hoặc `awk`. Tại sao lại thay đổi? Sự đơn giản và tích hợp. Có một giải pháp nội bộ làm cho việc thao tác chuỗi trở nên đơn giản hơn, khiến các script dễ đọc và bảo trì hơn.

Có lựa chọn khác không? Chắc chắn, `sed` cũ kỹ vẫn có thể làm được việc:

```Fish Shell
set old_school_string "Fish@Shell2023"
echo $old_school_string | sed 's/[0-9]//g'
# Kết quả: Fish@Shell
```

Nhưng tại sao không tận dụng công cụ của chính Fish? Để thực hiện, `string replace` có tùy chọn `-r` cho phép sử dụng mẫu regex. `-a` áp dụng lệnh cho tất cả các trận đấu, và thêm một '' ở cuối báo hiệu cho nó thay thế bằng không cái gì, tức là xóa. Sử dụng `string match` khi tìm kiếm một mẫu cần giữ, thay vì cái cần bỏ.

## Xem thêm
- Tài liệu chính thức của Fish Shell về `string`: https://fishshell.com/docs/current/cmds/string.html
- Hướng dẫn Regex để đi sâu vào các mẫu: https://www.regular-expressions.info/
- Sed & Awk, sức mạnh văn bản cổ điển: giới thiệu: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:39.263001-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Bi\u1EC3u th\u1EE9c ch\xEDnh quy \u0111\
  \xE3 c\xF3 t\u1EEB nh\u1EEFng n\u0103m 1950, ban \u0111\u1EA7u \u0111\u01B0\u1EE3\
  c ngh\u0129 ra b\u1EDFi nh\xE0 to\xE1n h\u1ECDc Stephen Kleene. C\xE1c ph\u01B0\u01A1\
  ng \xE1n thay th\u1EBF cho regex trong\u2026"
lastmod: '2024-04-05T21:53:38.235401-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy \u0111\xE3 c\xF3 t\u1EEB nh\u1EEFng n\u0103\
  m 1950, ban \u0111\u1EA7u \u0111\u01B0\u1EE3c ngh\u0129 ra b\u1EDFi nh\xE0 to\xE1\
  n h\u1ECDc Stephen Kleene."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Cách thực hiện:
```Bash
# Tìm kiếm một mẫu
echo "I love to code in Bash" | grep -oP 'code'

# Đầu ra:
code

# Thay thế chuỗi sử dụng regex với sed
echo "Bash 2023" | sed -E 's/[0-9]+/2024/'

# Đầu ra:
Bash 2024

# Trích xuất chuỗi con bằng regex
echo "Error: Line 42" | grep -oP '(?<=Line )\d+'

# Đầu ra:
42
```

## Sâu hơn
Biểu thức chính quy đã có từ những năm 1950, ban đầu được nghĩ ra bởi nhà toán học Stephen Kleene. Các phương án thay thế cho regex trong Bash bao gồm việc sử dụng `awk` hoặc `perl`, mỗi người có khả năng regex riêng của mình. Về mặt thực hiện, Bash sử dụng grep cho việc tìm kiếm, `sed` cho tìm kiếm và thay thế, và toán tử `=~` trong `[[ ]]` cho điều kiện. Hãy lưu ý rằng regex có thể khác biệt giữa các công cụ (`grep`, `egrep`, `sed`, và `awk`), vì vậy cần biết rõ loại bạn đang làm việc.

## Xem thêm
- [GNU Grep Manual](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed - Một Giới thiệu và Hướng dẫn](https://www.grymoire.com/Unix/Sed.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex101: Online Regex Tester và Debugger](https://regex101.com/)

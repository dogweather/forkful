---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:39.263001-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u1ECBnh ngh\u0129a ti\xEAu ch\xED t\xECm ki\u1EBFm cho v\u0103n b\u1EA3n. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, thay\
  \ th\u1EBF, ho\u1EB7c tr\xEDch xu\u1EA5t c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.863240-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u1ECBnh ngh\u0129a ti\xEAu ch\xED t\xECm ki\u1EBFm cho v\u0103n b\u1EA3n."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Cái gì và Tại sao?
Biểu thức chính quy (regex) là những mẫu định nghĩa tiêu chí tìm kiếm cho văn bản. Lập trình viên sử dụng chúng để tìm kiếm, thay thế, hoặc trích xuất các phần từ chuỗi dựa trên những mẫu này—nghĩ đến việc tìm và thay thế phức tạp được nâng cấp.

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

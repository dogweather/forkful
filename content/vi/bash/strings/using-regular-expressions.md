---
title:                "Sử dụng biểu thức chính quy"
aliases: - /vi/bash/using-regular-expressions.md
date:                  2024-01-28T22:09:39.263001-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

---
title:                "Sử dụng biểu thức chính quy"
aliases:
- /vi/python/using-regular-expressions/
date:                  2024-01-28T22:09:49.833020-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là những mẫu được sử dụng để khớp các kết hợp ký tự trong chuỗi. Lập trình viên sử dụng regex để tìm kiếm, chỉnh sửa hoặc thao tác với văn bản vì nó mạnh mẽ và hiệu quả.

## Làm thế nào:
Dưới đây là các ví dụ Python sử dụng mô-đun `re` cho các hoạt động regex phổ biến:

```Python
import re

# Tìm tất cả các trận đấu của 'abc' trong một chuỗi
matches = re.findall('abc', 'abc123abc')
print(matches)  # Đầu ra: ['abc', 'abc']

# Tìm kiếm 'def' và trả về một đối tượng Match
match = re.search('def', '123def456')
if match:
    print(match.group())  # Đầu ra: 'def'

# Thay thế 'ghi' bằng 'xyz'
replaced = re.sub('ghi', 'xyz', 'ghi123ghi')
print(replaced)  # Đầu ra: 'xyz123xyz'
```

## Sâu hơn
Biểu thức chính quy đã tồn tại từ những năm 1950, được phát triển cùng với lý thuyết ngôn ngữ hình thức. Những phương án thay thế cho regex bao gồm thư viện phân tích cú pháp và các phương thức chuỗi như `str.find()` hoặc `str.replace()`, nhưng những phương án này thiếu sự linh hoạt trong việc khớp mẫu của regex. Về mặt triển khai, Python sử dụng mô-đun `re`, dựa trên thư viện regex UNIX truyền thống nhưng bao gồm một số cải tiến.

## Xem thêm
- Tài liệu mô-đun `re` của Python: https://docs.python.org/3/library/re.html
- Hướng dẫn cú pháp biểu thức chính quy: https://www.regular-expressions.info/
- Công cụ kiểm tra và gỡ lỗi Regex: https://regex101.com/

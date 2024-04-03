---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:49.833020-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1c v\xED\
  \ d\u1EE5 Python s\u1EED d\u1EE5ng m\xF4-\u0111un `re` cho c\xE1c ho\u1EA1t \u0111\
  \u1ED9ng regex ph\u1ED5 bi\u1EBFn."
lastmod: '2024-03-13T22:44:36.083550-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1c v\xED d\u1EE5 Python s\u1EED d\u1EE5\
  ng m\xF4-\u0111un `re` cho c\xE1c ho\u1EA1t \u0111\u1ED9ng regex ph\u1ED5 bi\u1EBF\
  n."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

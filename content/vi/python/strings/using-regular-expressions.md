---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:49.833020-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p\
  \ k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex\
  \ \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa ho\u1EB7c thao\u2026"
lastmod: '2024-03-13T22:44:36.083550-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p\
  \ k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex\
  \ \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa ho\u1EB7c thao\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

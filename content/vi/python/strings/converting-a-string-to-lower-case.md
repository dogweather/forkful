---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:30.989902-07:00
description: "Chuy\u1EC3n m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng c\xF3\
  \ ngh\u0129a l\xE0 thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c k\xFD t\u1EF1 vi\u1EBF\
  t hoa trong v\u0103n b\u1EA3n th\xE0nh c\xE1c k\xFD t\u1EF1 t\u01B0\u01A1ng \u0111\
  \u01B0\u01A1ng \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. C\xE1c l\u1EADp\u2026"
lastmod: '2024-03-13T22:44:36.079703-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng c\xF3\
  \ ngh\u0129a l\xE0 thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c k\xFD t\u1EF1 vi\u1EBF\
  t hoa trong v\u0103n b\u1EA3n th\xE0nh c\xE1c k\xFD t\u1EF1 t\u01B0\u01A1ng \u0111\
  \u01B0\u01A1ng \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. C\xE1c l\u1EADp\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Gì & Tại sao?
Chuyển một chuỗi thành chữ thường có nghĩa là thay đổi tất cả các ký tự viết hoa trong văn bản thành các ký tự tương đương ở dạng chữ thường. Các lập trình viên thường làm điều này vì lý do nhất quán, so sánh hoặc tìm kiếm, vì 'A' không giống như 'a' trong thế giới máy tính.

## Làm thế nào:
Chuyển một chuỗi sang dạng chữ thường trong Python rất đơn giản với phương thức `.lower()`.
```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()
print(lowercase_string)  # Kết quả: hello, world!
```
Hoặc sử dụng hiểu biết danh sách để kiểm soát nhiều hơn:
```Python
s = "HELLO, World!"
lower_list = [char.lower() for char in s]
print(''.join(lower_list))  # Kết quả: hello, world!
```

## Sâu hơn
Phương thức `.lower()` đã là một phần của kiểu chuỗi Python từ khá sớm. Đây là cách đơn giản để đảm bảo xử lý dữ liệu không phân biệt chữ hoa chữ thường, điều này hữu ích trong các tình huống như đầu vào của người dùng không phân biệt chữ hoa chữ thường.

Có những phương pháp thay thế, như sử dụng biểu thức chính quy:
```Python
import re

s = "HELLO, World!"
lower_s = re.sub(r'[A-Z]', lambda match: match.group(0).lower(), s)
print(lower_s)  # Kết quả: hello, world!
```
Nhưng đây là quá mức cho việc đơn giản là chuyển một chuỗi thành chữ thường.

Bên dưới lớp vỏ, phương pháp `.lower()` của Python dựa trên bản đồ ký tự Unicode. Tiêu chuẩn Unicode xác định ký tự tương đương chữ thường của hầu hết tất cả các ký tự có dạng chữ. Quy trình này phức tạp hơn là chỉ cần trừ một giá trị để từ 'A' chuyển thành 'a' bởi vì không phải tất cả ngôn ngữ và kịch bản đều có sự ánh xạ trực tiếp và đơn giản như vậy.

## Xem thêm
- Tài liệu Python về các phương thức chuỗi: https://docs.python.org/3/library/stdtypes.html#string-methods
- Chi tiết ánh xạ chữ hoa chữ thường Unicode: https://www.unicode.org/reports/tr21/tr21-5.html
- Một hướng dẫn về hiểu biết danh sách Python: https://realpython.com/list-comprehension-python/

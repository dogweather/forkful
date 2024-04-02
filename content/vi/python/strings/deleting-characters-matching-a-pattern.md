---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:16.125781-07:00
description: "Trong l\u1EADp tr\xECnh, vi\u1EC7c x\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDB\
  p v\u1EDBi m\u1ED9t m\u1EABu ngh\u0129a l\xE0 t\xECm c\xE1c chu\u1ED7i k\xFD t\u1EF1\
  \ ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t quy t\u1EAFc c\u1EE5 th\u1EC3\u2014m\u1ED9t\
  \ m\u1EABu\u2014v\xE0 lo\u1EA1i b\u1ECF ch\xFAng kh\u1ECFi chu\u1ED7i. C\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.075835-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1EADp tr\xECnh, vi\u1EC7c x\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDB\
  p v\u1EDBi m\u1ED9t m\u1EABu ngh\u0129a l\xE0 t\xECm c\xE1c chu\u1ED7i k\xFD t\u1EF1\
  \ ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t quy t\u1EAFc c\u1EE5 th\u1EC3\u2014m\u1ED9t\
  \ m\u1EABu\u2014v\xE0 lo\u1EA1i b\u1ECF ch\xFAng kh\u1ECFi chu\u1ED7i. C\xE1c\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Gì và Tại sao?
Trong lập trình, việc xóa các ký tự khớp với một mẫu nghĩa là tìm các chuỗi ký tự phù hợp với một quy tắc cụ thể—một mẫu—và loại bỏ chúng khỏi chuỗi. Các lập trình viên thực hiện điều này cho các tác vụ như làm sạch đầu vào, xử lý văn bản, hoặc chỉ đơn giản là dọn dẹp dữ liệu trước khi lưu trữ hoặc hiển thị nó.

## Cách làm:
```Python
import re

# Chuỗi ví dụ
text = "Hello, World! 1234"

# Xóa tất cả chữ số
khong_chu_so = re.sub(r'\d', '', text)
print(khong_chu_so)  # Kết quả: "Hello, World! "

# Xóa dấu câu
khong_dau_cau = re.sub(r'[^\w\s]', '', text)
print(khong_dau_cau)  # Kết quả: "Hello World 1234"

# Xóa nguyên âm
khong_nguyen_am = re.sub(r'[aeiouAEIOU]', '', text)
print(khong_nguyen_am)  # Kết quả: "Hll, Wrld! 1234"
```

## Tìm hiểu sâu
Thực hành xóa các ký tự khớp với một mẫu trong văn bản có gốc rễ sâu xa trong khoa học máy tính, bắt đầu từ những công cụ Unix đầu tiên như `sed` và `grep`. Trong Python, mô-đun `re` cung cấp khả năng này, sử dụng biểu thức chính quy—một công cụ mạnh mẽ và linh hoạt cho xử lý văn bản.

Các phương án thay thế cho mô-đun `re` bao gồm:
- Các phương thức chuỗi như `replace()` cho các trường hợp đơn giản.
- Các thư viện bên thứ ba như `regex` cho các mẫu phức tạp hơn và hỗ trợ Unicode tốt hơn.

Bên dưới cùng, khi bạn sử dụng `re.sub()`, trình thông dịch Python biên dịch mẫu thành một loạt các bytecodes, được xử lý bởi một máy trạng thái thực hiện khớp mẫu trực tiếp trên văn bản đầu vào. Thao tác này có thể tốn kém tài nguyên cho các chuỗi lớn hoặc các mẫu phức tạp, do đó việc xem xét hiệu suất là rất quan trọng cho xử lý dữ liệu lớn.

## Xem thêm
- [Tài liệu mô-đun `re` của Python](https://docs.python.org/3/library/re.html): Tài liệu chính thức về biểu thức chính quy trong Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Hướng dẫn toàn diện về biểu thức chính quy.
- [Hướng dẫn về regex của Real Python](https://realpython.com/regex-python/): Ứng dụng thực tế của biểu thức chính quy trong Python.

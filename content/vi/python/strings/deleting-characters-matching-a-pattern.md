---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-04-04T01:28:03.834003-06:00'
model: gpt-4-0125-preview
summary: .
title: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Cách thực hiện:
```Python
import re

# Chuỗi ví dụ
text = "Hello, World! 1234"

# Xóa tất cả các chữ số
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # Kết quả: "Hello, World! "

# Xóa dấu câu
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # Kết quả: "Hello World 1234"

# Xóa nguyên âm
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # Kết quả: "Hll, Wrld! 1234"
```

### Một hàm tùy chỉnh do tôi viết

Tôi làm điều này đủ thường xuyên đến mức tôi đã chuyển nó thành hàm `delete()` này. Đây cũng là một ví dụ tốt về [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## Sâu hơn nữa
Thực hành xóa các ký tự khớp với một mẫu trong văn bản có nguồn gốc sâu xa trong khoa học máy tính, trở lại từ những công cụ Unix đầu tiên như `sed` và `grep`. Trong Python, mô-đun `re` cung cấp khả năng này, tận dụng biểu thức chính quy - một công cụ mạnh mẽ và linh hoạt cho việc xử lý văn bản.

Các phương án thay thế cho mô-đun `re` bao gồm:
- Các phương thức chuỗi như `replace()` cho các trường hợp đơn giản.
- Các thư viện bên thứ ba như `regex` cho các mẫu phức tạp hơn và hỗ trợ Unicode tốt hơn.

Bên dưới cơ chế, khi bạn sử dụng `re.sub()`, trình thông dịch Python biên dịch mẫu thành một loạt các bytecodes, được xử lý bởi một máy trạng thái thực hiện so khớp mẫu trực tiếp trên văn bản đầu vào. Thao tác này có thể tiêu tốn tài nguyên với chuỗi lớn hoặc các mẫu phức tạp, do đó xem xét hiệu suất là rất quan trọng cho việc xử lý dữ liệu lớn.

## Xem thêm
- [Tài liệu mô-đun `re` của Python](https://docs.python.org/3/library/re.html): Tài liệu chính thức về biểu thức chính quy trong Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Hướng dẫn toàn diện về biểu thức chính quy.
- [Hướng dẫn về regex của Real Python](https://realpython.com/regex-python/): Ứng dụng thực tế của biểu thức chính quy trong Python.

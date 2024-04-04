---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-04-04T02:02:41.806017-06:00'
model: gpt-4-0125-preview
summary: .
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
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

### Hàm tùy chỉnh của tôi

Tôi thực hiện việc này đủ thường xuyên nên tôi đã tái cấu trúc nó thành hàm `delete()` đơn giản này. Đây cũng là một ví dụ tốt về [doctests](https://docs.python.org/3/library/doctest.html):

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



## Sâu hơn
Thực hành xóa các ký tự khớp với một mẫu trong văn bản có nguồn gốc sâu xa trong khoa học máy tính, truy nguyên lại đến những công cụ Unix đầu tiên như `sed` và `grep`. Trong Python, module `re` cung cấp khả năng này, tận dụng biểu thức chính quy - một công cụ mạnh mẽ và linh hoạt cho xử lí văn bản.

Các phương án thay thế cho module `re` bao gồm:
- Các phương thức chuỗi như `replace()` cho các trường hợp đơn giản.
- Các thư viện bên thứ ba như `regex` cho các mẫu phức tạp hơn và hỗ trợ Unicode tốt hơn.

Ở dưới cơ, khi bạn sử dụng `re.sub()`, trình thông dịch Python biên dịch mẫu thành một loạt các bytecodes, được xử lý bởi một máy trạng thái thực hiện việc khớp mẫu trực tiếp trên văn bản đầu vào. Thao tác này có thể tốn tài nguyên với chuỗi lớn hoặc mẫu phức tạp, vì vậy việc cân nhắc về hiệu suất là rất quan trọng đối với việc xử lý dữ liệu lớn.

## Xem thêm
- [Tài liệu module `re` của Python](https://docs.python.org/3/library/re.html): Tài liệu chính thức về biểu thức chính quy trong Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Hướng dẫn toàn diện về biểu thức chính quy.
- [Hướng dẫn về regex của Real Python](https://realpython.com/regex-python/): Ứng dụng thực tế của biểu thức chính quy trong Python.

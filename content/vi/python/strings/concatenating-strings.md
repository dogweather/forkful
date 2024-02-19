---
aliases:
- /vi/python/concatenating-strings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.538216-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1\
  o th\xE0nh m\u1ED9t chu\u1ED7i m\u1EDBi. N\xF3 gi\u1ED1ng nh\u01B0 Lego chu\u1ED7\
  i. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng v\u0103\
  n\u2026"
lastmod: 2024-02-18 23:08:50.258974
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1i v\u1EDB\
  i nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1o th\xE0\
  nh m\u1ED9t chu\u1ED7i m\u1EDBi. N\xF3 gi\u1ED1ng nh\u01B0 Lego chu\u1ED7i. Ch\xFA\
  ng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng v\u0103n\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối để tạo thành một chuỗi mới. Nó giống như Lego chuỗi. Chúng ta làm điều này để xây dựng văn bản; hãy nghĩ về tên người dùng, thông báo lỗi, và nội dung động.

## Cách thực hiện:
Hãy ghép một số chuỗi lại với nhau.

```python
first_name = "Charlie"
last_name = "Brown"
full_name = first_name + " " + last_name  # Nối chuỗi cổ điển với một khoảng trắng
print(full_name)
```
Đầu ra: `Charlie Brown`

Sử dụng `join()` cho một danh sách từ:

```python
words = ["Hello", "world!"]
sentence = " ".join(words)
print(sentence)
```
Đầu ra: `Hello world!`

F-String (từ Python 3.6):

```python
user = "snoopy"
action = "flying"
log_message = f"{user} đang {action} nhà của mình"
print(log_message)
```
Đầu ra: `snoopy đang bay nhà của mình`

## Sâu hơn nữa
Nối chuỗi đã là một hoạt động cơ bản của chuỗi kể từ bình minh của lập trình. Nhớ rằng, Python xử lý chuỗi như là không thể thay đổi, vì vậy mỗi lần nối tạo ra một chuỗi mới.

Một thời, dấu cộng (`+`) là tất cả những gì chúng ta có. Không hiệu quả cho nhiều chuỗi, vì nó có thể dẫn đến việc bùng phát bộ nhớ và hiệu suất chậm. Đến lượt phương thức `join()`—thân thiện với bộ nhớ hơn, đặc biệt cho việc kết hợp một loạt chuỗi.

F-Strings, được giới thiệu trong Python 3.6, là một bước ngoặt. Chúng dễ đọc và nhanh chóng và cho phép đánh giá biểu thức trong các literan chuỗi—`f"{biến}"`. Chúng là lựa chọn hàng đầu cho một Pythonista hiện đại, kết hợp chức năng và hiệu quả.

## Xem thêm
- [Phương thức Chuỗi Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- Nội suy Chuỗi Literan](https://www.python.org/dev/peps/pep-0498/)
- [Thực hành Tốt nhất về Định dạng Chuỗi Python](https://realpython.com/python-f-strings/)

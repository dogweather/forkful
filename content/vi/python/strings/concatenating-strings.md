---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.538216-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y gh\xE9p m\u1ED9t s\u1ED1 chu\u1ED7\
  i l\u1EA1i v\u1EDBi nhau."
lastmod: '2024-03-13T22:44:36.086086-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y gh\xE9p m\u1ED9t s\u1ED1 chu\u1ED7i l\u1EA1i v\u1EDBi nhau."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

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

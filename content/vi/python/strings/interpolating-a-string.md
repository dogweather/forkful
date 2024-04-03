---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:23.369031-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Python 3.6 tr\u1EDF l\xEAn, b\u1EA1n c\xF3\
  \ th\u1EC3 n\u1ED9i suy chu\u1ED7i s\u1EED d\u1EE5ng f-strings. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1ch."
lastmod: '2024-03-13T22:44:36.078422-06:00'
model: gpt-4-0125-preview
summary: "Trong Python 3.6 tr\u1EDF l\xEAn, b\u1EA1n c\xF3 th\u1EC3 n\u1ED9i suy chu\u1ED7\
  i s\u1EED d\u1EE5ng f-strings."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Python 3.6 trở lên, bạn có thể nội suy chuỗi sử dụng f-strings. Dưới đây là cách:

```Python
name = 'Alice'
age = 30
greeting = f"Xin chào, {name}. Bạn {age} tuổi."

print(greeting)
```

Đầu ra:
```
Xin chào, Alice. Bạn 30 tuổi.
```

Bạn cũng có thể sử dụng biểu thức bên trong dấu ngoặc nhọn:

```Python
a = 5
b = 10
info = f"Năm cộng mười là {a + b}, không phải {2 * (a + b)}."

print(info)
```

Đầu ra:
```
Năm cộng mười là 15, không phải 30.
```

## Sâu hơn
Trước Python 3.6, `.format()` là cách thức được áp dụng để nội suy chuỗi:

```Python
name = 'Bob'
age = 25
greeting = "Xin chào, {}. Bạn {} tuổi.".format(name, age)

print(greeting)
```

Python cũ (phiên bản < 2.6) sử dụng toán tử `%` cho nội suy, ít trực quan và có thể trở nên lộn xộn với nhiều biến:

```Python
name = 'Carol'
age = 35
greeting = "Xin chào, %s. Bạn %d tuổi." % (name, age)

print(greeting)
```

Ngoài cú pháp sạch sẽ hơn, f-strings còn nhanh hơn vì chúng được đánh giá tại thời điểm chạy và sau đó được chuyển đổi trực tiếp thành một thao tác định dạng chuỗi hiệu quả. Phương pháp `.format()` và toán tử `%` bao gồm nhiều bước hơn và chậm hơn.

## Xem thêm
- [PEP 498 – Nội suy chuỗi literan](https://www.python.org/dev/peps/pep-0498/) để biết tài liệu chính thức về f-strings.
- [Python f-strings](https://realpython.com/python-f-strings/) của Real Python cho một bài hướng dẫn về sử dụng f-strings.
- [Phương pháp .format()](https://docs.python.org/3/library/stdtypes.html#str.format) trong tài liệu Python để hiểu rõ hơn về phương pháp định dạng chuỗi `.format()` cũ.

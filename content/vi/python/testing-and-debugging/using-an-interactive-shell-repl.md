---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:16.763799-07:00
description: "L\xE0m th\u1EBF n\xE0o: B\u1EAFt \u0111\u1EA7u ngay v\u1EDBi REPL c\u1EE7\
  a Python b\u1EB1ng c\xE1ch g\xF5 `python` trong d\xF2ng l\u1EC7nh c\u1EE7a b\u1EA1\
  n. M\u1ED9t khi \u0111\xE3 v\xE0o, h\xE3y th\u1EED c\xE1c thao t\xE1c \u0111\u01A1\
  n gi\u1EA3n ho\u1EB7c m\xE3 nhi\u1EC1u\u2026"
lastmod: '2024-03-13T22:44:36.099164-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u ngay v\u1EDBi REPL c\u1EE7a Python b\u1EB1ng c\xE1\
  ch g\xF5 `python` trong d\xF2ng l\u1EC7nh c\u1EE7a b\u1EA1n."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Làm thế nào:
Bắt đầu ngay với REPL của Python bằng cách gõ `python` trong dòng lệnh của bạn. Một khi đã vào, hãy thử các thao tác đơn giản hoặc mã nhiều dòng:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

Thử nghiệm với các hàm và phản hồi ngay lập tức:

```Python
>>> def greet(name):
...     return "Xin chào, " + name + "!"
... 
>>> greet("Alice")
'Xin chào, Alice!'
```

Chơi với các thư viện và khám phá tính năng của chúng trong thời gian thực:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Thoát nhanh với một lệnh `exit()` hoặc `Ctrl+D` (đôi khi là `Ctrl+Z` trên Windows).

## Đào sâu
Khái niệm về REPL không chỉ độc quyền cho Python; nó cổ xưa như Lisp. Nhiều ngôn ngữ cung cấp môi trường tương tác, tức thì này cho một cách tiếp cận trực tiếp đối với việc mã hóa. Các phương án thay thế cho shell Python gốc bao gồm IPython và Jupyter Notebook, chúng cung cấp sự tương tác cao cấp hơn, nhiều tính năng hơn, và tích hợp tốt hơn với các công cụ khác. REPL chuẩn của Python đơn giản, nhưng nó kết hợp đầy đủ sức mạnh của Python, xử lý các đối tượng phức tạp và chương trình đa luồng, mặc dù nó thiếu các tính năng như tự động hoàn thành và nổi bật cú pháp có trong các công cụ nâng cao hơn.

## Xem thêm
- [Tài liệu chính thức của Python về trình thông dịch](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: Một shell Python nâng cao](https://ipython.org/)
- [Dự án Jupyter](https://jupyter.org/)

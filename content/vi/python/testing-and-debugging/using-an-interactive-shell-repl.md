---
aliases:
- /vi/python/using-an-interactive-shell-repl/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:16.763799-07:00
description: "REPL, vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop (V\xF2ng l\u1EB7\
  p \u0110\u1ECDc-T\xEDnh to\xE1n-In), l\xE0 m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EAD\
  p tr\xECnh l\u1EA5y \u0111\u1EA7u v\xE0o \u0111\u01A1n l\u1EBB t\u1EEB ng\u01B0\u1EDD\
  i d\xF9ng, th\u1EF1c thi ch\xFAng v\xE0 tr\u1EA3\u2026"
lastmod: 2024-02-18 23:08:50.269912
model: gpt-4-0125-preview
summary: "REPL, vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop (V\xF2ng l\u1EB7\
  p \u0110\u1ECDc-T\xEDnh to\xE1n-In), l\xE0 m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EAD\
  p tr\xECnh l\u1EA5y \u0111\u1EA7u v\xE0o \u0111\u01A1n l\u1EBB t\u1EEB ng\u01B0\u1EDD\
  i d\xF9ng, th\u1EF1c thi ch\xFAng v\xE0 tr\u1EA3\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
REPL, viết tắt của Read-Eval-Print Loop (Vòng lặp Đọc-Tính toán-In), là một môi trường lập trình lấy đầu vào đơn lẻ từ người dùng, thực thi chúng và trả lại kết quả cho người dùng. Lập trình viên sử dụng nó để thử nghiệm nhanh, học tập, gỡ lỗi, hoặc thực hiện các phép tính tức thời.

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

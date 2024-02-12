---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- /vi/python/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:10:16.763799-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

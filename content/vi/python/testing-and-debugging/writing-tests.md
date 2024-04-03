---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:04.709450-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y s\u1EED d\u1EE5ng b\u1ED9 khung\
  \ `unittest` c\xF3 s\u1EB5n c\u1EE7a Python."
lastmod: '2024-03-13T22:44:36.101784-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y s\u1EED d\u1EE5ng b\u1ED9 khung `unittest` c\xF3 s\u1EB5n c\u1EE7\
  a Python."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cách thực hiện:
Hãy sử dụng bộ khung `unittest` có sẵn của Python.

```Python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add_integers(self):
        self.assertEqual(add(1, 2), 3)

    def test_add_strings(self):
        self.assertEqual(add('abc', 'def'), 'abcdef')

if __name__ == '__main__':
    unittest.main()
```

Chạy nó, bạn sẽ thấy thứ gì đó như:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```

Hai dấu chấm có nghĩa là hai bài kiểm thử đã vượt qua. Mọi thứ đều tốt.

## Sâu hơn nữa
Việc kiểm thử Python bắt đầu trở nên phổ biến với `unittest` (lấy cảm hứng từ JUnit của Java). Bây giờ, có `pytest` và `nose`, những công cụ hiện đại hơn với cú pháp đơn giản và tính năng tốt hơn. Khi viết kiểm thử, hãy nhớ: cô lập các trường hợp kiểm thử, kiểm tra các trường hợp cực biên, và giả lập sự phụ thuộc bên ngoài để tập trung vào logic mã của bạn, không phải thế giới bên ngoài.

## Xem thêm
Khám phá sâu hơn vào việc kiểm thử với những cái này:

- Tài liệu `unittest` của Python: https://docs.python.org/3/library/unittest.html
- `pytest` cho một cách tiếp cận hiện đại hơn: https://docs.pytest.org/en/latest/
- Giả lập trong kiểm thử với `unittest.mock`: https://docs.python.org/3/library/unittest.mock.html

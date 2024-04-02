---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:04.709450-07:00
description: "Vi\u1EC7c vi\u1EBFt ki\u1EC3m th\u1EED t\u1EE9c l\xE0 t\u1EA1o ra m\xE3\
  \ \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng \u0111\
  \xFAng kh\xF4ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 b\u1EAFt\
  \ l\u1ED7i, \u0111\u1EA3m b\u1EA3o \u0111\u1ED9 tin c\u1EADy, v\xE0 l\xE0m cho vi\u1EC7\
  c\u2026"
lastmod: '2024-03-13T22:44:36.101784-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt ki\u1EC3m th\u1EED t\u1EE9c l\xE0 t\u1EA1o ra m\xE3\
  \ \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng \u0111\
  \xFAng kh\xF4ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 b\u1EAFt\
  \ l\u1ED7i, \u0111\u1EA3m b\u1EA3o \u0111\u1ED9 tin c\u1EADy, v\xE0 l\xE0m cho vi\u1EC7\
  c\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cái gì & Tại sao?

Việc viết kiểm thử tức là tạo ra mã để kiểm tra xem mã khác có hoạt động đúng không. Chúng ta làm điều này để bắt lỗi, đảm bảo độ tin cậy, và làm cho việc cập nhật ít đáng sợ hơn.

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

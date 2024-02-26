---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:23.369031-07:00
description: "N\u1ED9i suy chu\u1ED7i l\xE0 ph\u01B0\u01A1ng ph\xE1p nh\xFAng c\xE1\
  c bi\u1EC3u th\u1EE9c v\xE0o b\xEAn trong c\xE1c chu\u1ED7i literan. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 ch\xE8n \u0111\u1ED9ng c\xE1c gi\xE1\
  \ tr\u1ECB v\xE0o chu\u1ED7i, l\xE0m cho m\xE3\u2026"
lastmod: '2024-02-25T18:49:34.453669-07:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i l\xE0 ph\u01B0\u01A1ng ph\xE1p nh\xFAng c\xE1c bi\u1EC3\
  u th\u1EE9c v\xE0o b\xEAn trong c\xE1c chu\u1ED7i literan. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 ch\xE8n \u0111\u1ED9ng c\xE1c gi\xE1 tr\u1ECB\
  \ v\xE0o chu\u1ED7i, l\xE0m cho m\xE3\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Nội suy chuỗi là phương pháp nhúng các biểu thức vào bên trong các chuỗi literan. Lập trình viên sử dụng nó để chèn động các giá trị vào chuỗi, làm cho mã lệnh trở nên dễ đọc và sạch sẽ hơn so với nối chuỗi truyền thống.

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

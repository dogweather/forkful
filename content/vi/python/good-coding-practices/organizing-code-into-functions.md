---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.258157-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7\
  c chia nh\u1ECF m\xE3 c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng v\u1EDBi c\xE1c m\u1EE5c \u0111\xEDch c\u1EE5 th\u1EC3\
  . Ch\xFAng ta l\xE0m \u0111i\u1EC1u \u0111\xF3 \u0111\u1EC3 l\xE0m cho m\xE3 s\u1EA1\
  ch\u2026"
lastmod: '2024-03-13T22:44:36.104616-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7c chia\
  \ nh\u1ECF m\xE3 c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3 t\xE1\
  i s\u1EED d\u1EE5ng v\u1EDBi c\xE1c m\u1EE5c \u0111\xEDch c\u1EE5 th\u1EC3. Ch\xFA\
  ng ta l\xE0m \u0111i\u1EC1u \u0111\xF3 \u0111\u1EC3 l\xE0m cho m\xE3 s\u1EA1ch\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức mã thành các hàm là về việc chia nhỏ mã của bạn thành các khối có thể tái sử dụng với các mục đích cụ thể. Chúng ta làm điều đó để làm cho mã sạch sẽ hơn, dễ đọc, dễ gỡ lỗi và cập nhật hơn.

## Làm thế nào:
Giả sử bạn đang viết một script để tính bình phương và lập phương của một số. Không có hàm, nó sẽ là một mớ hỗn độn lặp lại:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Bình phương: {square}, Lập phương: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Bình phương: {square}, Lập phương: {cube}")
```
Đầu ra:
```
Bình phương: 16, Lập phương: 64
Bình phương: 25, Lập phương: 125
```

Với hàm, nó sẽ gọn gàng hơn:

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"Bình phương: {square(num)}, Lập phương: {cube(num)}")

num = 5
print(f"Bình phương: {square(num)}, Lập phương: {cube(num)}")
```
Đầu ra:
```
Bình phương: 16, Lập phương: 64
Bình phương: 25, Lập phương: 125
```

## Sâu hơn
Ngày xưa, khi các chương trình còn đơn giản, bạn có thể thoát khỏi việc chỉ viết một danh sách các chỉ thị. Nhưng khi phần mềm trở nên phức tạp hơn, các nhà phát triển nhận ra họ đang viết đi viết lại cùng một đoạn mã. Xin chào, hàm—các khối mã có thể tái sử dụng thực hiện một hành động duy nhất.

Các phương án thay thế cho hàm bao gồm lớp (gói hàm với dữ liệu mà chúng hoạt động trên) và mã nội tuyến (trí tuệ ngay nơi bạn cần, nhưng rủi ro cho các nhiệm vụ phức tạp). Về mặt thực hiện, mẹo không chỉ là tạo hàm mà là làm cho chúng thực hiện một việc tốt—nghĩ về nguyên tắc trách nhiệm đơn lẻ. Các hàm cũng lý tưởng nên không có trạng thái, có nghĩa là không có bất ngờ với dữ liệu đầu vào hoặc đầu ra.

## Xem thêm
- Hướng dẫn chính thức của Python về hàm: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' của Robert C. Martin, về các nguyên tắc viết hàm sạch sẽ.
- 'Refactoring: Improving the Design of Existing Code' của Martin Fowler, bao gồm các ví dụ về tổ chức mã.

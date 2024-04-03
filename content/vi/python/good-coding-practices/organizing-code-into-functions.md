---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.258157-07:00
description: "L\xE0m th\u1EBF n\xE0o: Gi\u1EA3 s\u1EED b\u1EA1n \u0111ang vi\u1EBF\
  t m\u1ED9t script \u0111\u1EC3 t\xEDnh b\xECnh ph\u01B0\u01A1ng v\xE0 l\u1EADp ph\u01B0\
  \u01A1ng c\u1EE7a m\u1ED9t s\u1ED1. Kh\xF4ng c\xF3 h\xE0m, n\xF3 s\u1EBD l\xE0 m\u1ED9\
  t m\u1EDB h\u1ED7n \u0111\u1ED9n l\u1EB7p l\u1EA1i."
lastmod: '2024-03-13T22:44:36.104616-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n \u0111ang vi\u1EBFt m\u1ED9t script \u0111\u1EC3\
  \ t\xEDnh b\xECnh ph\u01B0\u01A1ng v\xE0 l\u1EADp ph\u01B0\u01A1ng c\u1EE7a m\u1ED9\
  t s\u1ED1."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

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

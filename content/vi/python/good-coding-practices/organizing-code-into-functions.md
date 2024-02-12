---
title:                "Sắp xếp mã thành các hàm"
aliases:
- /vi/python/organizing-code-into-functions/
date:                  2024-01-28T22:03:42.258157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

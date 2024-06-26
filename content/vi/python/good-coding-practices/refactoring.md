---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:01.033921-07:00
description: "L\xE0m th\u1EBF n\xE0o: Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t \u0111\
  o\u1EA1n code t\xEDnh to\xE1n v\xE0 in ra di\u1EC7n t\xEDch v\xE0 chu vi c\u1EE7\
  a m\u1ED9t h\xECnh ch\u1EEF nh\u1EADt khi bi\u1EBFt chi\u1EC1u d\xE0i v\xE0 chi\u1EC1\
  u r\u1ED9ng. N\xF3 th\u1EF1c hi\u1EC7n c\xF4ng\u2026"
lastmod: '2024-03-13T22:44:36.108570-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t \u0111o\u1EA1n code t\xEDnh to\xE1\
  n v\xE0 in ra di\u1EC7n t\xEDch v\xE0 chu vi c\u1EE7a m\u1ED9t h\xECnh ch\u1EEF\
  \ nh\u1EADt khi bi\u1EBFt chi\u1EC1u d\xE0i v\xE0 chi\u1EC1u r\u1ED9ng."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
Giả sử bạn có một đoạn code tính toán và in ra diện tích và chu vi của một hình chữ nhật khi biết chiều dài và chiều rộng. Nó thực hiện công việc, nhưng có phần lặp lại và hơi lộn xộn.

```python
# Phiên bản Gốc
length = 4
width = 3

# Tính toán diện tích và chu vi
area = length * width
perimeter = 2 * (length + width)

print("Diện Tích:", area)
print("Chu Vi:", perimeter)
```

Chúng ta có thể tái cấu trúc bằng cách đóng gói chức năng vào trong các hàm, làm cho code trở nên có tổ chức và có thể sử dụng lại hơn:

```python
# Phiên bản Đã Refactor

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# sử dụng
length = 4
width = 3

print("Diện Tích:", calculate_area(length, width))
print("Chu Vi:", calculate_perimeter(length, width))
```

Cả hai đoạn code đều cho ra kết quả giống nhau:
```
Diện Tích: 12
Chu Vi: 14
```

Nhưng phiên bản đã tái cấu trúc sạch sẽ hơn và tách biệt các mối quan tâm, làm cho việc cập nhật một cái tính toán mà không ảnh hưởng đến cái kia trở nên dễ dàng hơn.

## Sâu hơn nữa
Refactoring có nguồn gốc từ những ngày đầu của kỹ thuật phần mềm khi các lập trình viên nhận ra rằng code có thể và nên được cải thiện ngay cả khi nó đã "hoạt động". Cuốn sách quan trọng của Martin Fowler "Refactoring: Improving the Design of Existing Code" đã nêu bật nhiều nguyên tắc và kỹ thuật cốt lõi. Ông nổi tiếng đã nói, "Bất kỳ kẻ ngốc nào cũng có thể viết code mà máy tính có thể hiểu. Lập trình viên giỏi viết code mà con người có thể hiểu."

Các phương án thay thế cho việc tái cấu trúc có thể bao gồm viết lại code từ đầu hoặc thực hiện những điều chỉnh nhỏ mà không cải thiện một cách có hệ thống. Tuy nhiên, tái cấu trúc thường hiệu quả về chi phí hơn so với việc viết lại và ít rủi ro hơn so với các điều chỉnh không có kế hoạch. Chi tiết thực thi có thể cụ thể cho từng ngữ cảnh lập trình; tuy nhiên, lập trình hướng đối tượng đặc biệt phù hợp với việc tái cấu trúc, đặc biệt với các kỹ thuật như trích xuất các phương thức (như các hàm `calculate_area` và `calculate_perimeter` của chúng ta), nội suy, di chuyển tính năng giữa các đối tượng, và đổi tên các phương thức hoặc biến để rõ ràng hơn.

Tái cấu trúc trong Python thường sử dụng các công cụ như `PyCharm`, có khả năng tái cấu trúc tích hợp, hoặc `rope`, một thư viện Python được thiết kế đặc biệt cho việc tái cấu trúc. Việc sử dụng cẩn thận kiểm soát phiên bản, như `git`, trong quá trình tái cấu trúc được khuyến khích mạnh mẽ để theo dõi các thay đổi một cách dần dần.

## Xem thêm
Cho những ai muốn tìm hiểu sâu hơn, hãy tham khảo các tài nguyên sau:
- Sách của Martin Fowler: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Tái cấu trúc Python với `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- Tài liệu tái cấu trúc PyCharm: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Tái cấu trúc và Mẫu thiết kế](https://refactoring.guru/refactoring)
- Các bài giảng về Code sạch của Uncle Bob (Robert C. Martin): [Code Sạch - Uncle Bob / Bài học 1](https://www.youtube.com/watch?v=7EmboKQH8lM)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:57.340341-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9\
  nh ch\xFAng \u0111\u1EC3 ch\xFAng g\u1EA7n v\u1EDBi m\u1ED9t gi\xE1 tr\u1ECB \u0111\
  \u01A1n gi\u1EA3n ho\u1EB7c quan tr\u1ECDng h\u01A1n. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\u01A1n gi\u1EA3n h\xF3a k\u1EBFt\u2026"
lastmod: '2024-03-13T22:44:36.089925-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh\
  \ ch\xFAng \u0111\u1EC3 ch\xFAng g\u1EA7n v\u1EDBi m\u1ED9t gi\xE1 tr\u1ECB \u0111\
  \u01A1n gi\u1EA3n ho\u1EB7c quan tr\u1ECDng h\u01A1n."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Gì và Tại Sao?
Làm tròn số có nghĩa là điều chỉnh chúng để chúng gần với một giá trị đơn giản hoặc quan trọng hơn. Các lập trình viên làm tròn số để đơn giản hóa kết quả, giới hạn số chữ số thập phân để hiển thị hoặc vì một số mục đích toán học nhất định.

## Cách thực hiện:
Dưới đây là thông tin cơ bản về việc làm tròn số trong Python:

```python
# Làm tròn số về số nguyên gần nhất
print(round(8.67))  # Đầu ra: 9

# Làm tròn số với số chữ số thập phân được chỉ định
print(round(8.67, 1))  # Đầu ra: 8.7

# Số chẵn được làm tròn xuống và số lẻ được làm tròn lên khi có khoảng cách bằng nhau
print(round(2.5))  # Đầu ra: 2
print(round(3.5))  # Đầu ra: 4
```

## Sâu hơn nữa
Trong Python, `round()` không chỉ đơn giản là cắt bỏ phần thập phân. Theo truyền thống, Python, giống như nhiều ngôn ngữ khác, tuân theo "làm tròn một nửa về số chẵn" hoặc "làm tròn ngân hàng". Điều này giúp giảm thiểu lỗi tích lũy trong tổng hoặc trung bình, điều này rất quan trọng trong tính toán tài chính.

Đối với các lựa chọn thay thế, bạn có `math.floor()` và `math.ceil()` từ module toán học của Python, kéo số về dưới hoặc lên về số nguyên tiếp theo. Nhưng nếu bạn muốn độ chính xác, `quantize()` của module `decimal` cho phép bạn chỉ định hành vi làm tròn.

Bên dưới lớp vỏ, `round()` xử lý với số dấu phẩy động nhị phân. Do một số số thập phân không thể được biểu diễn chính xác bằng nhị phân, bạn có thể gặp bất ngờ với những trường hợp như `round(2.675, 2)` không trở thành `2.68` như mong đợi. Sử dụng `decimal` hoặc `fractions` để đạt được độ chính xác cao.

## Xem thêm
- Tài liệu của Python về các hàm tích hợp: https://docs.python.org/3/library/functions.html#round
- Toán học với số điểm cố định và số dấu phẩy động: https://docs.python.org/3/library/decimal.html
- Module toán học của Python: https://docs.python.org/3/library/math.html

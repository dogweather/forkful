---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:17.848877-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Vi\u1EC7c l\xE0m tr\xF2n s\u1ED1 kh\xF4\
  ng ph\u1EA3i l\xE0 m\u1EDBi\u2014lo\xE0i ng\u01B0\u1EDDi \u0111\xE3 l\xE0m \u0111\
  i\u1EC1u n\xE0y t\u1EEB h\xE0ng th\u1EBF k\u1EF7 \u0111\u1EC3 l\xE0m cho c\xE1c\
  \ ph\xE9p t\xEDnh d\u1EC5 d\xE0ng h\u01A1n ho\u1EB7c \u0111\u1EC3 l\xE0m vi\u1EC7\
  c trong\u2026"
lastmod: '2024-04-05T22:50:51.612612-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\xE0m tr\xF2n s\u1ED1 kh\xF4ng ph\u1EA3i l\xE0 m\u1EDBi\u2014\
  lo\xE0i ng\u01B0\u1EDDi \u0111\xE3 l\xE0m \u0111i\u1EC1u n\xE0y t\u1EEB h\xE0ng\
  \ th\u1EBF k\u1EF7 \u0111\u1EC3 l\xE0m cho c\xE1c ph\xE9p t\xEDnh d\u1EC5 d\xE0\
  ng h\u01A1n ho\u1EB7c \u0111\u1EC3 l\xE0m vi\u1EC7c trong gi\u1EDBi h\u1EA1n c\u1EE7\
  a c\xE1c c\xF4ng c\u1EE5 c\u1EE7a h\u1ECD."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách thực hiện:
```Ruby
# Làm tròn cơ bản
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Chỉ định độ chính xác
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Làm tròn xuống
puts 2.9.floor          # => 2

# Làm tròn lên
puts 2.1.ceil           # => 3

# Làm tròn về phía không
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Kết quả mẫu:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Tìm hiểu sâu
Việc làm tròn số không phải là mới—loài người đã làm điều này từ hàng thế kỷ để làm cho các phép tính dễ dàng hơn hoặc để làm việc trong giới hạn của các công cụ của họ. Trong Ruby, phương thức `round` là linh hoạt, có khả năng làm tròn đến số nguyên gần nhất mặc định hoặc đến một vị trí thập phân được chỉ định.

Một phương thức thay thế cho `round` là `floor` dùng để luôn làm tròn xuống, và `ceil` để luôn làm tròn lên, bất kể giá trị của số. Để chỉ cắt bỏ các chữ số thập phân, bạn có `truncate`.

Về mặt lịch sử, khi nói đến máy tính, việc làm tròn trở nên quan trọng trong việc xử lý số học dấu phẩy động do độ chính xác không hoàn hảo của nó. Ruby, giống như hầu hết các ngôn ngữ, tuân theo tiêu chuẩn IEEE 754 cho số dấu phẩy động, có nghĩa là nó xử lý việc làm tròn theo cách mà hầu hết lập trình viên có thể dự đoán và tin tưởng.

Tuy nhiên, còn nhiều hơn thế nữa—những thứ như làm tròn theo ngân hàng (còn được gọi là làm tròn một nửa đến số chẵn) là các khái niệm mà các nhà phát triển Ruby có thể cần phải triển khai thủ công, vì phương thức `round` không cung cấp sẵn nó.

## Xem thêm
- [Tài liệu Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) cho phương thức `round` của Float.
- [Tiêu chuẩn IEEE về Số học Dấu phẩy động (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Hiểu về Độ chính xác Số Dấu phẩy động](https://floating-point-gui.de/), để có cái nhìn sâu sắc hơn về cách máy tính xử lý số thập phân.

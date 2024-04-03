---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:43.970297-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Fish, vi\u1EC7c l\xE0m tr\xF2n s\u1ED1\
  \ d\u1EF1a v\xE0o c\xE2u l\u1EC7nh `math`. S\u1EED d\u1EE5ng `math -s0` \u0111\u1EC3\
  \ l\xE0m tr\xF2n \u0111\u1EBFn s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t."
lastmod: '2024-03-13T22:44:37.201125-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, vi\u1EC7c l\xE0m tr\xF2n s\u1ED1 d\u1EF1a v\xE0o c\xE2u l\u1EC7\
  nh `math`."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm thế nào:
Trong Fish, việc làm tròn số dựa vào câu lệnh `math`. Sử dụng `math -s0` để làm tròn đến số nguyên gần nhất.

```fish
# Làm tròn lên
echo (math -s0 "4.7")
# Kết quả: 5

# Làm tròn xuống
echo (math -s0 "4.3")
# Kết quả: 4

# Làm tròn đến hai chữ số thập phân
echo (math -s2 "4.5678")
# Kết quả: 4.57

# Làm tròn số âm
echo (math -s0 "-2.5")
# Kết quả: -3
```

## Sâu hơn
Trong lịch sử, việc làm tròn số được thực hiện một cách thủ công hơn hoặc với công cụ bên ngoài, nhưng trong các shells hiện đại như Fish, nó đã được tích hợp sẵn vào các tiện ích cốt lõi. Cách tiếp cận của Fish bằng việc sử dụng câu lệnh `math` đơn giản hóa so với các shell cũ. Các phương án thay thế trong môi trường lập trình khác biệt; ngôn ngữ như Python sử dụng các hàm như `round()`, trong khi Bash có thể yêu cầu biểu thức phức tạp hơn hoặc tiện ích `bc`. Cách thực hiện làm tròn số của Fish đơn giản hóa việc viết script bằng cách giữ các phép toán toán học bên trong môi trường shell thay vì gọi các công cụ hoặc ngôn ngữ khác.

## Xem thêm
- Tài liệu Fish về câu lệnh `math`: https://fishshell.com/docs/current/cmds/math.html
- Tiêu chuẩn IEEE cho Số học Điểm Nổi (IEEE 754): https://ieeexplore.ieee.org/document/4610935

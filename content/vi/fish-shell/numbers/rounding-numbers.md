---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:43.970297-07:00
description: "L\xE0m tr\xF2n s\u1ED1 l\xE0 vi\u1EC7c lo\u1EA1i b\u1ECF c\xE1c ch\u1EEF\
  \ s\u1ED1 th\u1EADp ph\xE2n \u0111\u1EC3 \u0111\u01A1n gi\u1EA3n h\xF3a d\u1EEF\
  \ li\u1EC7u ho\u1EB7c ph\xF9 h\u1EE3p v\u1EDBi c\xE1c \u0111\u1ECBnh d\u1EA1ng c\u1EE5\
  \ th\u1EC3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 hi\u1EC3n\u2026"
lastmod: 2024-02-19 22:04:56.434320
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 l\xE0 vi\u1EC7c lo\u1EA1i b\u1ECF c\xE1c ch\u1EEF\
  \ s\u1ED1 th\u1EADp ph\xE2n \u0111\u1EC3 \u0111\u01A1n gi\u1EA3n h\xF3a d\u1EEF\
  \ li\u1EC7u ho\u1EB7c ph\xF9 h\u1EE3p v\u1EDBi c\xE1c \u0111\u1ECBnh d\u1EA1ng c\u1EE5\
  \ th\u1EC3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 hi\u1EC3n\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Làm tròn số là việc loại bỏ các chữ số thập phân để đơn giản hóa dữ liệu hoặc phù hợp với các định dạng cụ thể. Lập trình viên thực hiện việc này để hiển thị thân thiện với người dùng, lưu trữ hiệu quả, hoặc khi độ chính xác của số thập phân không phải là vấn đề.

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

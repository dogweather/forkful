---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.408850-07:00
description: "C\xE1ch th\u1EE9c: Bash kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c\
  \ m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn. B\u1EA1n th\u01B0\u1EDDng s\u1EED d\u1EE5ng\
  \ m\u1ED9t c\xF4ng c\u1EE5 b\xEAn ngo\xE0i nh\u01B0 `bc` v\u1EDBi t\xF9y ch\u1ECD\
  n `-l` c\u1EE7a n\xF3. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n\u2026"
lastmod: '2024-03-13T22:44:36.868321-06:00'
model: gpt-4-0125-preview
summary: "Bash kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c m\u1ED9t c\xE1ch t\u1EF1\
  \ nhi\xEAn."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Cách thức:
Bash không hỗ trợ số phức một cách tự nhiên. Bạn thường sử dụng một công cụ bên ngoài như `bc` với tùy chọn `-l` của nó. Dưới đây là cách bạn xử lý số phức trong bash:

```bash
echo "sqrt(-1)" | bc -l
```

Kết quả:
```bash
j
```

Phép nhân:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Kết quả:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Sâu Hơn
Số phức đã xuất hiện từ thế kỷ 16, nhưng các ngôn ngữ kịch bản như Bash không được chuẩn bị sẵn sàng cho các phép toán toán học như số phức ngay từ ban đầu. Đó là lý do tại sao `bc` hoặc các công cụ khác như `awk` thường được sử dụng. Một số ngôn ngữ thay thế để làm việc với số phức bao gồm Python với module `cmath` và MATLAB, đều được xây dựng cho các chức năng toán học nâng cao hơn. Đối với Bash, đó là cách tận dụng các công cụ - `bc` sử dụng chữ 'i' viết thường để biểu diễn đơn vị ảo và hỗ trợ các phép toán cơ bản như cộng, trừ, nhân, chia.

## Xem Thêm
- Hướng dẫn sử dụng `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (thay thế cho MATLAB): https://www.gnu.org/software/octave/
- Module `cmath` của Python: https://docs.python.org/3/library/cmath.html

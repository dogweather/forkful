---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.408850-07:00
description: "S\u1ED1 ph\u1EE9c bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\
  \ trong c\xE1c l\u0129nh v\u1EF1c nh\u01B0 x\u1EED l\xFD t\xEDn hi\u1EC7u, c\u01A1\
  \ h\u1ECDc l\u01B0\u1EE3ng t\u1EED v\xE0 b\u1EA5t c\u1EE9 khi n\xE0o\u2026"
lastmod: '2024-03-13T22:44:36.868321-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\
  \ trong c\xE1c l\u0129nh v\u1EF1c nh\u01B0 x\u1EED l\xFD t\xEDn hi\u1EC7u, c\u01A1\
  \ h\u1ECDc l\u01B0\u1EE3ng t\u1EED v\xE0 b\u1EA5t c\u1EE9 khi n\xE0o\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Số Phức: Làm Thế Nào và Tại Sao?

Số phức bao gồm một phần thực và một phần ảo. Các lập trình viên sử dụng chúng trong các lĩnh vực như xử lý tín hiệu, cơ học lượng tử và bất cứ khi nào tính toán yêu cầu, bởi vì số thực thông thường không đủ đáp ứng.

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

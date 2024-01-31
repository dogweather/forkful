---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:12:22.408850-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

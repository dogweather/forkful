---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:34.312788-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 th\xF4\
  ng tin chi ti\u1EBFt v\u1EC1 l\xE0m tr\xF2n trong Bash."
lastmod: '2024-03-13T22:44:36.869599-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 th\xF4ng tin chi ti\u1EBFt v\u1EC1 l\xE0\
  m tr\xF2n trong Bash."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách thực hiện:
Dưới đây là thông tin chi tiết về làm tròn trong Bash:

```Bash
# Làm tròn xuống sử dụng 'floor' với bc
echo "scale=0; 3.49/1" | bc

# Làm tròn lên sử dụng 'ceiling' với bc
echo "scale=0; 3.01/1" | bc -l

# Làm tròn đến số nguyên gần nhất sử dụng printf
printf "%.0f\n" 3.49

# Một mẹo để làm tròn đến số nguyên gần nhất sử dụng bc
echo "(3.49+0.5)/1" | bc
```

Kết quả mẫu—trực tiếp từ terminal:

```
3  # Làm tròn xuống (floor)
4  # Làm tròn lên (ceiling)
3  # Làm tròn đến gần nhất (với printf)
3  # Làm tròn đến gần nhất (với bc)
```

## Sâu hơn nữa
Ngày xưa, không có `bc` hoặc `printf` trong kịch bản Bash để thực hiện phép toán diệu kỳ. Người dùng cổ điển phải dựa vào công cụ bên ngoài hoặc tìm tòi giải pháp sáng tạo. Bây giờ, `bc` cho phép bạn thực hiện toán học chính xác. Hãy lưu ý, `bc` không làm tròn theo mặc định—nó làm tròn xuống. Phần scale đặt hành động cho điểm thập phân.

Có cách thay thế? Bạn có thể sử dụng `awk` để làm tròn mà không cần chuyển qua `bc` hoặc đấu tranh với `perl` cho nhu cầu toán học nặng nề hơn. Dành cho những người quá khích, hãy đi với Bash thuần túy, chẳng hạn, thao tác chuỗi lặp lại – nhưng tại sao?

Về chi tiết, `bc` không chỉ làm tròn, nó thực hiện rất nhiều thứ toán học—tính tỉ lệ, hàm sin, căn bậc hai, bạn tên nó. Với `printf`, đó là nhiều hơn về định dạng văn bản, nhưng này, nó cũng làm tròn số, vì vậy chúng tôi không phàn nàn.

## Xem thêm
Dành cho những ai muốn tìm hiểu thêm:

- Hướng dẫn sử dụng GNU `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Lệnh `printf` trong Bash: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Hướng dẫn sử dụng người dùng AWK (cho việc làm tròn và xử lý văn bản khác): https://www.gnu.org/software/gawk/manual/gawk.html
- Thêm về toán học, viết kịch bản và mẹo số học trong Bash: https://mywiki.wooledge.org/BashFAQ/022

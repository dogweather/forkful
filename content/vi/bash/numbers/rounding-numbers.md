---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:34.312788-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 c\u1EAFt b\u1ECF ph\u1EA7\
  n th\u1EADp ph\xE2n \u0111\u1EC3 sinh ra m\u1ED9t gi\xE1 tr\u1ECB \u0111\u01A1n\
  \ gi\u1EA3n h\u01A1n ph\xF9 h\u1EE3p v\u1EDBi ng\u1EEF c\u1EA3nh c\u1EE5 th\u1EC3\
  . C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\u01A1\
  n gi\u1EA3n\u2026"
lastmod: 2024-02-19 22:04:56.058226
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 c\u1EAFt b\u1ECF ph\u1EA7\
  n th\u1EADp ph\xE2n \u0111\u1EC3 sinh ra m\u1ED9t gi\xE1 tr\u1ECB \u0111\u01A1n\
  \ gi\u1EA3n h\u01A1n ph\xF9 h\u1EE3p v\u1EDBi ng\u1EEF c\u1EA3nh c\u1EE5 th\u1EC3\
  . C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\u01A1\
  n gi\u1EA3n\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Làm thế nào và Tại sao?

Làm tròn số có nghĩa là cắt bỏ phần thập phân để sinh ra một giá trị đơn giản hơn phù hợp với ngữ cảnh cụ thể. Các lập trình viên làm tròn số để đơn giản hóa kết quả, tiết kiệm không gian hoặc bởi vì giá trị chính xác không quan trọng—như khi bạn ước lượng sử dụng CPU hoặc dung lượng đĩa, và phần thập phân không làm thay đổi ngày của bạn.

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

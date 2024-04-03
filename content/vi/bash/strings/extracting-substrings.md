---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:43.082611-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1i nh\xEC\
  n t\u1ED5ng quan v\u1EC1 tr\xEDch xu\u1EA5t chu\u1ED7i con trong Bash."
lastmod: '2024-03-13T22:44:36.862000-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1i nh\xECn t\u1ED5ng quan v\u1EC1 tr\xED\
  ch xu\u1EA5t chu\u1ED7i con trong Bash."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Dưới đây là cái nhìn tổng quan về trích xuất chuỗi con trong Bash:

```Bash
# Sử dụng ${string:start:length}
text="The quick brown fox"
substring=${text:4:5}
echo $substring  # Xuất ra 'quick'

# Độ dài mặc định là phần còn lại của chuỗi
substring=${text:16}
echo $substring  # Xuất ra 'fox'

# Chỉ mục bắt đầu âm (tính từ cuối chuỗi)
substring=${text: -3}
echo $substring  # Xuất ra 'fox'
```

## Sâu hơn nữa
Bash đã xử lý chuỗi từ rất lâu. Trích xuất chuỗi con là một mẹo cũ nhưng vẫn cực kỳ hữu dụng. Trước khi có những công cụ tiên tiến, chúng ta chỉ có mở rộng tham số – cú pháp `${}` – và nó đã chịu được thử thách của thời gian.

Có lựa chọn khác không? Chắc chắn rồi. `awk`, `cut`, và `grep` đều có thể cắt và chia nhỏ chuỗi theo cách riêng của chúng. Nhưng cho một công việc nhanh chóng, không cần kích hoạt thêm, phương pháp đúc kết sẵn trong Bash là hiệu quả.

Về mặt triển khai, Bash trích xuất chuỗi con một cách dễ dàng. Nó không quan tâm đến nội dung bên trong chuỗi của bạn là gì: văn bản, số, biểu tượng cảm xúc kỳ lân – bất cứ thứ gì. Chỉ cần cho nó điểm bắt đầu và kết thúc, và nó sẽ cắt đoạn đó mà không cần biết.

## Xem thêm
Đào sâu hơn và kiểm tra các liên kết sau:

- Hướng dẫn sử dụng Bash về mở rộng tham số: `man bash` và tìm kiếm *Mở Rộng Tham Số*
- `awk` và `grep` sâu hơn: [Hướng dẫn Awk](https://www.gnu.org/software/gawk/manual/) và [Sổ tay Grep](https://www.gnu.org/software/grep/manual/grep.html)
- Cái nhìn tổng quan hơn về thao tác chuỗi trong Bash: [Hướng dẫn Thao Tác Chuỗi Bash](https://www.tldp.org/LDP/abs/html/string-manipulation.html)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:43.082611-07:00
description: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra\
  \ c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a chu\u1ED7i \u2014 ngh\u0129 gi\u1ED1\
  ng nh\u01B0 vi\u1EC7c c\u1EAFt m\u1ED9t \xEDt s\u1EE3i ch\u1EC9 t\u1EEB m\u1ED9\
  t chi\u1EBFc \xE1o len. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u \u0111\xF3\
  \u2026"
lastmod: 2024-02-19 22:04:56.049827
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a chu\u1ED7i \u2014 ngh\u0129 gi\u1ED1ng nh\u01B0\
  \ vi\u1EC7c c\u1EAFt m\u1ED9t \xEDt s\u1EE3i ch\u1EC9 t\u1EEB m\u1ED9t chi\u1EBF\
  c \xE1o len. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u \u0111\xF3\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trích xuất chuỗi con có nghĩa là lấy ra các phần cụ thể của chuỗi — nghĩ giống như việc cắt một ít sợi chỉ từ một chiếc áo len. Lập trình viên làm điều đó để cô lập, phân tích hay thao tác với dữ liệu gắn liền trong văn bản.

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

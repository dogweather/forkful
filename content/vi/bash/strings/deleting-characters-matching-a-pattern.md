---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:11.242057-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: K\u1EBFt qu\u1EA3: `Xin ch\xE0o, Th\u1EBF\
  \ gi\u1EDBi!`."
lastmod: '2024-04-05T22:37:45.575110-06:00'
model: gpt-4-0125-preview
summary: "K\u1EBFt qu\u1EA3: `Xin ch\xE0o, Th\u1EBF gi\u1EDBi!`."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Cách thực hiện:


### Xóa khoảng trắng đầu/cuối:
```Bash
text="   Xin chào, Thế giới!   "
trimmed=$(echo "$text" | xargs)
echo "$trimmed"
```
Kết quả: `Xin chào, Thế giới!`

### Loại bỏ tất cả các chữ số:
```Bash
text="B4sh i5 tuyệt vờ1!"
cleaned=${text//[^a-zA-Z ]/}
echo "$cleaned"
```
Kết quả: `Bsh i tuyệt vờ`

### Thay thế các ký tự cụ thể:
```Bash
text="Xin chào-Thế giới!"
cleaned=${text//-/_}
echo "$cleaned"
```
Kết quả: `Xin chào_Thế giới!`

## Sâu hơn
Ban đầu, các công cụ xử lý văn bản như `sed` và `awk` là điểm đến cho việc thao tác chuỗi. Bash đã kể từ đó kết hợp khả năng phù hợp mẫu và thao tác chuỗi trực tiếp vào chính bảng điều khiển, cung cấp cho người dùng nhiều quyền lực mà không cần đến các lệnh bên ngoài.

Cú pháp `${parameter/pattern/string}` là một phương pháp, nơi bạn thay thế phù hợp đầu tiên của `pattern` bằng `string`. Để xóa tất cả các trận đấu, chỉ cần thêm một `/` nữa như đã hiển thị trong các ví dụ trên. 

Các phương án thay thế bao gồm việc sử dụng các công cụ UNIX cổ điển như `sed`, `awk`, `tr`, hoặc các ngôn ngữ kịch bản hiện đại hơn như Python hoặc Perl.

Phần cốt lõi, Bash sử dụng globbing và các ký tự đại diện cho phù hợp mẫu, nhưng khi bạn thấy những cấu trúc `${text//pattern/}`, bạn đang đối mặt với sự mở rộng tham số của Bash - một tính năng cực kỳ hữu ích để thao tác chuỗi.

## Xem Thêm
- Hướng dẫn Bash về Sự mở rộng Tham số: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Một bài viết về xử lý văn bản trong Linux: https://www.linuxjournal.com/content/pattern-matching-bash
- Sed & Awk 101 Hacks eBook: https://www.thegeekstuff.com/ebooks/sed_awk_101_hacks

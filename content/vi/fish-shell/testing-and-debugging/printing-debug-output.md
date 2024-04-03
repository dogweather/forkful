---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:00.912368-07:00
description: "C\xE1ch l\xE0m: L\xE0m quen v\u1EDBi `echo` - dao g\u0103m Th\u1EE5\
  y S\u0129 cho vi\u1EC7c xu\u1EA5t d\u1EEF li\u1EC7u trong Fish. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1ch \u0111\u1EC3 r\u1EA3i r\xE1c in \u1EA5n g\u1EE1 r\u1ED1i v\xE0\
  o c\xE1c k\u1ECBch b\u1EA3n shell c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:37.214112-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m quen v\u1EDBi `echo` - dao g\u0103m Th\u1EE5y S\u0129 cho vi\u1EC7\
  c xu\u1EA5t d\u1EEF li\u1EC7u trong Fish."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cách làm:
Làm quen với `echo` - dao găm Thụy Sĩ cho việc xuất dữ liệu trong Fish. Dưới đây là cách để rải rác in ấn gỡ rối vào các kịch bản shell của bạn.

```Fish Shell
function chào
    set tên $argv[1]
    echo "Này, $tên! Hãy cùng gỡ rối."
    echo "Đang chạy hàm chào" >&2
end

chào "Ada"
```
Dữ liệu ra mẫu:
```
Này, Ada! Hãy cùng gỡ rối.
Đang chạy hàm chào
```
Dữ liệu ra chuẩn (`stdout`) là sân khấu chính của kịch bản của bạn, nhưng để nhắc nhở khi gỡ rối, hãy sử dụng lỗi chuẩn (`stderr`) với `>&2`.

## Đào Sâu
Trở về khi màn hình máy tính sâu bằng chiều rộng, dữ liệu ra là thứ quý giá. Dữ liệu ra chuẩn (`stdout`) trở thành kênh thuần khiết hướng người dùng, trong khi lỗi chuẩn (`stderr`) trở thành con đường hẻm cho những thông tin gỡ rối chỉ dành cho lập trình viên.

Trong Fish, các lệnh chuẩn để xuất dữ liệu là `echo`, `printf`, và `print`. `Echo` đơn giản và thường được sử dụng cho các thông điệp đơn giản và gỡ rối ngay lập tức.

Bạn không chỉ giới hạn ở `echo`, dù vậy. Ưu tiên `printf` cho chuỗi đã định dạng, hoặc sử dụng chuyển hướng (`>` hoặc `>>`) để đổ thông tin gỡ rối vào một tệp để xem sau.

Về việc triển khai, việc sử dụng `stderr` cho dữ liệu ra gỡ rối là một quy ước từ thế giới Unix, giúp tách biệt sản lượng thực sự (output) khỏi tiếng ồn gỡ rối (debug noise). Điều này có nghĩa người dùng vẫn có thể chạy kịch bản ra của bạn mà không bị hỗn lộn với dữ liệu gỡ rối.

## Xem Thêm
- Tài liệu Fish Shell về [Lệnh](https://fishshell.com/docs/current/commands.html)
- StackOverflow: Thảo luận và ví dụ về [gỡ rối trong Fish](https://stackoverflow.com/questions/tagged/fish)
- Greg's Wiki: Thông tin chi tiết về [chuyển hướng I/O](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Redirection)

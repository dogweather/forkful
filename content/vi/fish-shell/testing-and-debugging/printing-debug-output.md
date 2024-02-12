---
title:                "In ra thông tin gỡ lỗi"
aliases:
- /vi/fish-shell/printing-debug-output.md
date:                  2024-01-28T22:05:00.912368-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
In ấn dữ liệu gỡ rối (debug output) tức là việc phát ra thông tin phụ trợ để giúp bạn hiểu code của mình đang làm gì. Lập trình viên làm việc này để phát hiện và sửa lỗi dễ dàng hơn.

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

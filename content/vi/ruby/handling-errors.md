---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:02:06.431511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Xử lý lỗi chính là việc dự phòng những điều không mong đợi trong code — quản lý lỗi một cách nhẹ nhàng mà không làm sập chương trình. Lập trình viên làm điều này để kiểm soát dòng chảy khi mọi thứ đi sai và để giữ trải nghiệm người dùng mượt mà.

## Làm Thế Nào:

Ruby sử dụng `begin`, `rescue`, `ensure`, và `end` để xử lý lỗi. Bạn bọc đoạn code rủi ro trong `begin` và `end`. Nếu xảy ra lỗi, `rescue` sẽ hoạt động.

```Ruby
begin
  # Đoạn code rủi ro ở đây.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oops! Bạn không thể làm điều đó: #{e.message}"
ensure
  puts "Cái này luôn luôn chạy, lỗi hay không."
end
```

Kết Quả Mẫu:
```
Oops! Bạn không thể làm điều đó: chia cho 0
Cái này luôn luôn chạy, lỗi hay không.
```

## Sâu Hơn

Lịch sử, xử lý lỗi trong ngôn ngữ lập trình đã phát triển đáng kể, với ngôn ngữ cổ điển thường có cơ chế thô sơ hoặc không tồn tại. Xử lý ngoại lệ của Ruby được lấy cảm hứng từ các ngôn ngữ như Python và Smalltalk.

Các phương thức khác để `begin-rescue` trong Ruby bao gồm sử dụng `rescue` trong định nghĩa phương thức hoặc sử dụng `throw` và `catch` cho kiểm soát dòng chảy không tiêu chuẩn, mặc dù chúng không được sử dụng cho xử lý lỗi điển hình.

Một chi tiết thú vị: ngoại lệ của Ruby là đối tượng (thể hiện của lớp `Exception` và các lớp con của nó), vì vậy bạn có thể định nghĩa các lớp lỗi tùy chỉnh và làm nhiều hơn là chỉ ghi lỗi — bạn có thể mang theo trạng thái phong phú trong chương trình cho việc xử lý lỗi mạnh mẽ hơn.

## Xem Thêm

- Tài liệu của Ruby về ngoại lệ và xử lý lỗi: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Hướng dẫn chi tiết về các phương pháp hay nhất khi xử lý lỗi trong Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)

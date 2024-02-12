---
title:                "In ra thông tin gỡ lỗi"
aliases: - /vi/ruby/printing-debug-output.md
date:                  2024-01-28T22:04:37.304676-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
In đầu ra để gỡ lỗi trong Ruby giống như việc để lại dấu vết trong mã của bạn để theo dõi giá trị của biến và dòng chảy của chương trình. Các lập trình viên làm điều này để phát hiện lỗi bằng cách kiểm tra code của họ đang làm gì ở các điểm khác nhau.

## Cách thực hiện:
Trong Ruby, `puts` và `p` là các phương thức bạn nên sử dụng khi muốn xuất đầu ra nhanh chóng lên console.

```Ruby
def who_said_what
  quote = "To be or not to be"
  author = "Shakespeare"
  puts "Quote: #{quote}"
  p "Said by: #{author}"
end

who_said_what
```

Đầu ra mẫu:

```
Quote: To be or not to be
"Said by: Shakespeare"
```

Phương thức `puts` in ra đầu ra dễ đọc cho con người, thêm một dòng mới ở cuối. Ngược lại, `p` in giá trị một cách thô sơ hơn, hữu ích khi bạn cần xem một cái gì đó có phải là chuỗi hay không.

## Sâu hơn
Trước đây, khi chưa có các IDE tiên tiến, việc in ra console chính là việc gỡ lỗi. Đây là một kỹ thuật cũ nhưng vàng, đặc biệt khi bạn muốn tránh tổn thất thời gian thiết lập một trình gỡ lỗi.

Là các phương án thay thế, bạn có thể sử dụng `pp` cho việc in đẹp các đối tượng phức tạp, hoặc các thư viện gem như `awesome_print` để tăng cường khả năng đọc. Nếu đầu ra gỡ lỗi của bạn trở nên quá nhiều, hãy cân nhắc sử dụng một thư viện log để kiểm soát mức độ chi tiết.

Về mặt triển khai, `puts` và `p` viết vào `$stdout`, một dòng I/O toàn cục trong Ruby. Đầu ra có thể được chuyển hướng nếu cần. Nhớ rằng, mặc dù những phương thức này tiện lợi, nhưng việc in quá nhiều có thể làm lộn xộn console và làm việc gỡ lỗi trở nên khó khăn hơn.

## Xem thêm
- Tài liệu Ruby cho `Kernel#puts`: https://ruby-doc.org/core/Kernel.html#method-i-puts
- Tài liệu Ruby cho `Kernel#p`: https://ruby-doc.org/core/Kernel.html#method-i-p
- Hướng dẫn về in đẹp trong Ruby: https://ruby-doc.org/stdlib/libdoc/pp/rdoc/PP.html
- Gem Awesome Print cho đầu ra ấn tượng: https://rubygems.org/gems/awesome_print/

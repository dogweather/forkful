---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
aliases:
- vi/ruby/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:35.086875-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Trong Ruby, việc gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc thêm tên người dùng và mật khẩu vào tiêu đề yêu cầu của bạn. Lập trình viên làm điều này để truy cập vào các nguồn tài nguyên đòi hỏi xác minh người dùng.

## Cách thực hiện:

Để gửi một yêu cầu HTTP với xác thực cơ bản, bạn thường sử dụng module `Net::HTTP` trong Ruby. Dưới đây là một ví dụ nhanh:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
username = 'your_username'
password = 'your_password'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

phản hồi = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(request)
}

puts phản hồi.body
```

Nếu bạn chạy mã này với thông tin đăng nhập hợp lệ, bạn sẽ thấy nội dung của phản hồi được in ra. Nếu thông tin đăng nhập không hợp lệ, bạn sẽ nhận được thông báo lỗi.

## Tìm hiểu kỹ hơn

Xác thực cơ bản có một lịch sử lâu dài trong các giao thức web, trở lại với các RFC đầu tiên định nghĩa hoạt động của internet. Đó là một phương pháp kiểm soát truy cập đơn giản: tên người dùng và mật khẩu được mã hóa bằng Base64 và truyền trong tiêu đề HTTP.

Tuy nhiên, xác thực cơ bản truyền thông tin đăng nhập dưới dạng văn bản thuần túy (mặc dù đã được mã hóa), vì vậy nó không an toàn qua HTTP. Sử dụng HTTPS sẽ tốt hơn để giữ thông tin đăng nhập an toàn khỏi những ánh mắt tò mò.

Có những phương pháp thay thế an toàn hơn như OAuth, thường được sử dụng cho xác thực API. OAuth cho phép người dùng ủy quyền truy cập của bên thứ ba mà không chia sẻ thông tin đăng nhập. Tuy nhiên, xác thực cơ bản vẫn được sử dụng, đặc biệt là cho các ứng dụng nội bộ và việc viết script nhanh chóng.

Một chi tiết cần lưu ý là `Net::HTTP` của Ruby không xử lý Xác thực Cơ bản nguyên thủy cho đến khi bạn rõ ràng sử dụng phương thức `basic_auth`. Cũng rất quan trọng để xử lý các ngoại lệ và phản hồi lỗi có thể xuất hiện từ yêu cầu HTTP.

## Xem Thêm

- Tài liệu thư viện tiêu chuẩn Ruby `Net::HTTP`: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, 'Lược đồ Xác thực HTTP "Cơ bản"': https://tools.ietf.org/html/rfc7617
- Giới thiệu về OAuth cho xác thực: https://oauth.net/2/
- Thêm thông tin về Ruby và yêu cầu HTTP: https://www.rubyguides.com/2019/08/ruby-http-request/

---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:09:03.689637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Gửi một yêu cầu HTTP có nghĩa là yêu cầu dữ liệu từ một tài nguyên trên web. Các lập trình viên làm điều này để tương tác với các API, lấy dữ liệu từ web, hoặc giao tiếp với máy chủ.

## Cách thực hiện:

Ruby làm cho việc gửi yêu cầu HTTP trở nên khá dễ dàng. Đây là cách nhanh nhất với thư viện chuẩn Net::HTTP.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

Điều này sẽ xuất nội dung HTML của `http://example.com`.

Bạn có thể muốn gửi dữ liệu nữa:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

Điều này gửi một yêu cầu POST với dữ liệu và hiển thị phản hồi.

## Sâu hơn:

Trước đây, gửi yêu cầu HTTP là khó khăn hơn và bạn có thể cần sử dụng một gem như `HTTParty`. Nhưng thư viện `Net::HTTP` tích hợp sẵn của Ruby đã phát triển rất nhiều. Hiện tại nó hỗ trợ hầu hết những gì bạn cần.

Tuy nhiên, `Net::HTTP` có thể rườm rà. Nếu dự án của bạn cần nhiều tính năng HTTP hoặc cú pháp đẹp hơn, `HTTParty` hoặc `Faraday` là những lựa chọn tuyệt vời. Những gems này cung cấp một API biểu cảm hơn và có thể xử lý các tình huống phức tạp hơn như middleware hoặc các adapter khác nhau.

Cơ bản, gửi một yêu cầu HTTP với Ruby bao gồm việc tạo một client HTTP, thiết lập một đối tượng yêu cầu với phương thức, tiêu đề và thân nếu cần, sau đó gửi yêu cầu để nhận phản hồi.

Ví dụ về HTTParty:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

Điều này làm cùng một việc với `Net::HTTP.get` nhưng với ít cấu hình hơn.

## Xem thêm:

Để có thông tin chi tiết hơn, tài liệu của Ruby rất hữu ích:
- Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

Và nếu bạn có hứng thú sâu sắc với mạng HTTP của Ruby, hãy xem qua:
- Ruby's Open URI: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- WebMock để kiểm tra yêu cầu HTTP: https://github.com/bblimke/webmock

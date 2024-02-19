---
aliases:
- /vi/ruby/sending-an-http-request/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:03.689637-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP c\xF3 ngh\u0129a l\xE0 y\xEA\
  u c\u1EA7u d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t t\xE0i nguy\xEAn tr\xEAn web. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1\
  ng t\xE1c v\u1EDBi c\xE1c API, l\u1EA5y d\u1EEF li\u1EC7u t\u1EEB web,\u2026"
lastmod: 2024-02-18 23:08:51.281280
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP c\xF3 ngh\u0129a l\xE0 y\xEAu c\u1EA7\
  u d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t t\xE0i nguy\xEAn tr\xEAn web. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1\
  c v\u1EDBi c\xE1c API, l\u1EA5y d\u1EEF li\u1EC7u t\u1EEB web,\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
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

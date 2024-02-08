---
title:                "Tải trang web"
aliases:
- vi/ruby/downloading-a-web-page.md
date:                  2024-01-28T21:59:49.168064-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải một trang web có nghĩa là lấy nội dung HTML từ internet. Các lập trình viên làm điều đó để phân tích dữ liệu, thu thập thông tin, hoặc theo dõi các thay đổi một cách có chương trình.

## Làm thế nào:
Ruby làm cho việc tải một trang web trở nên đơn giản với các thư viện như `net/http` và gems như `open-uri`. Dưới đây là cách làm bằng `net/http`:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com') 
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Bạn sẽ nhận được nội dung HTML của `http://example.com` được in ra.

Sử dụng `open-uri` còn đơn giản hơn:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

Một lần nữa, nội dung của trang web được hiển thị trên terminal của bạn.

## Tìm hiểu sâu hơn
Quay lại những ngày đầu của web, việc tải một trang là một việc khá tốn công, bao gồm việc tạo yêu cầu HTTP một cách thủ công. Ngày nay, Ruby đã làm giảm bớt phần lớn sự phức tạp đó.

Các phương án thay thế cho `net/http` và `open-uri` bao gồm các gems cấp cao hơn như `HTTParty` và `RestClient`. Chúng cung cấp nhiều tính năng hơn và một phương pháp hướng đối tượng. Đối với việc thu thập dữ liệu web cần nhiều công sức, nhiều Rubyist chuyển sang sử dụng `Nokogiri` để phân tích HTML hoặc `Mechanize` hoạt động như một trình duyệt web.

Khi nói đến việc triển khai, hãy nhớ rằng `open-uri` là một bộ bọc cho `net/http`, vì vậy nó khá tiện lợi nhưng có thể thiếu một số kiểm soát cấp thấp. `net/http` cho bạn nhiều kiểm soát hơn về yêu cầu nhưng có thể rườm rà cho các nhiệm vụ đơn giản.

## Xem thêm
Để đọc thêm và các nguồn lực bổ sung, hãy kiểm tra:

- Tài liệu Net::HTTP của Ruby: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Tài liệu Open-URI: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Trang web của Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Kho gem của Mechanize: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- Gem HTTParty trên GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Gem RestClient: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)

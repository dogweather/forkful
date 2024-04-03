---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:49.168064-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ruby l\xE0m cho vi\u1EC7c t\u1EA3i m\u1ED9t\
  \ trang web tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n v\u1EDBi c\xE1c th\u01B0 vi\u1EC7\
  n nh\u01B0 `net/http` v\xE0 gems nh\u01B0 `open-uri`. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 c\xE1ch l\xE0m b\u1EB1ng\u2026"
lastmod: '2024-03-13T22:44:37.338888-06:00'
model: gpt-4-0125-preview
summary: "Ruby l\xE0m cho vi\u1EC7c t\u1EA3i m\u1ED9t trang web tr\u1EDF n\xEAn \u0111\
  \u01A1n gi\u1EA3n v\u1EDBi c\xE1c th\u01B0 vi\u1EC7n nh\u01B0 `net/http` v\xE0 gems\
  \ nh\u01B0 `open-uri`."
title: "T\u1EA3i trang web"
weight: 42
---

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

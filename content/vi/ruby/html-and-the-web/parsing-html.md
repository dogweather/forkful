---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:33.249923-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1\
  p HTML trong Ruby, h\xE3y c\xE0i \u0111\u1EB7t gem 'Nokogiri' b\u1EB1ng c\xE2u l\u1EC7\
  nh `gem install nokogiri`. Nokogiri gi\u1ED1ng nh\u01B0 m\u1ED9t c\xF4ng c\u1EE5\
  \ \u0111a\u2026"
lastmod: '2024-03-13T22:44:37.337634-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Ruby, h\xE3y c\xE0\
  i \u0111\u1EB7t gem 'Nokogiri' b\u1EB1ng c\xE2u l\u1EC7nh `gem install nokogiri`."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Cách thực hiện:
Để phân tích cú pháp HTML trong Ruby, hãy cài đặt gem 'Nokogiri' bằng câu lệnh `gem install nokogiri`. Nokogiri giống như một công cụ đa năng cho việc làm việc với HTML và XML trong Ruby. Dưới đây là một ví dụ nhanh:

```ruby
require 'nokogiri'
require 'open-uri'

# Tải nội dung HTML từ một trang web
html_content = URI.open('http://example.com').read

# Phân tích cú pháp HTML
doc = Nokogiri::HTML(html_content)

# Trích xuất tiêu đề
title = doc.xpath('//title').text
puts "Tiêu đề của trang là: #{title}"
```

Điều này sẽ cho kết quả như sau: `Tiêu đề của trang là: Example Domain`.

## Đi sâu hơn
Trong những ngày đầu của Ruby, các lựa chọn để phân tích cú pháp HTML bị hạn chế. REXML được tích hợp sẵn nhưng chậm. Sau đó, Hpricot xuất hiện nhưng nhanh chóng biến mất. Nokogiri ra mắt vào năm 2008, kết hợp sự dễ dàng của Hpricot với tốc độ và sức mạnh của libxml, một bộ công cụ XML đã được chứng minh.

Trong thế giới phân tích cú pháp, luôn luôn có những lựa chọn thay thế. Một số người tuyên thệ với thư viện 'rexml' được tích hợp sẵn hoặc 'oga', một trình phân tích cú pháp XML/HTML khác cho Ruby. Nhưng Nokogiri vẫn được yêu thích vì sự mạnh mẽ và tốc độ của nó, chưa kể đến vô vàn tính năng đa dạng của nó.

Bên trong, Nokogiri chuyển đổi HTML thành một Mô hình Đối tượng Tài liệu (DOM)—một cấu trúc dạng cây. Điều này làm cho việc điều hướng và thao tác các phần tử trở nên dễ dàng. Sử dụng XPath và các bộ chọn CSS, bạn có thể xác định chính xác bất kỳ thông tin nào bạn cần.

## Xem thêm
- Gem Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Tài liệu rexml của Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Trình phân tích cú pháp thay thế 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Tìm hiểu về XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)

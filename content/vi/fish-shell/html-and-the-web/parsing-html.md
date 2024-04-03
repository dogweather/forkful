---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.009542-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish Shell kh\xF4ng ph\u1EA3i l\xE0 l\u1EF1\
  a ch\u1ECDn h\xE0ng \u0111\u1EA7u \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML,\
  \ nh\u01B0ng v\u1EDBi c\xE1c c\xF4ng c\u1EE5 ph\xF9 h\u1EE3p, vi\u1EC7c n\xE0y c\xF3\
  \ th\u1EC3 th\u1EF1c hi\u1EC7n \u0111\u01B0\u1EE3c. H\xE3y s\u1EED\u2026"
lastmod: '2024-03-13T22:44:37.207738-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell kh\xF4ng ph\u1EA3i l\xE0 l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7\
  u \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML, nh\u01B0ng v\u1EDBi c\xE1c c\xF4\
  ng c\u1EE5 ph\xF9 h\u1EE3p, vi\u1EC7c n\xE0y c\xF3 th\u1EC3 th\u1EF1c hi\u1EC7n\
  \ \u0111\u01B0\u1EE3c."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Làm thế nào:
Fish Shell không phải là lựa chọn hàng đầu để phân tích cú pháp HTML, nhưng với các công cụ phù hợp, việc này có thể thực hiện được. Hãy sử dụng `pup`, một trình phân tích cú pháp HTML dòng lệnh, để làm việc với nội dung HTML.

```fish
# Đầu tiên, cài đặt pup
brew install pup

# Lấy tiêu đề từ example.com
curl -s http://example.com | pup 'title text{}'

# Đầu ra mẫu nên là tiêu đề của trang web, ví dụ như:
# Example Domain
```

Bây giờ, hãy thu thập tất cả các liên kết siêu văn bản:

```fish
# Trích xuất liên kết (thuộc tính href) từ example.com
curl -s http://example.com | pup 'a attr{href}'

# Đầu ra mẫu:
# http://www.iana.org/domains/example
```

## Sâu hơn nữa
Trước khi có Fish Shell và `pup`, mọi người thường sử dụng regex rườm rà hoặc các kịch bản phía máy chủ phức tạp. Công cụ như `pup` đã làm cho quá trình này thông minh hơn, dựa vào cú pháp bộ chọn CSS cho việc phân tích cú pháp trực quan và đáng tin cậy hơn.

Các lựa chọn thay thế bao gồm Beautiful Soup của Python hoặc Node.js với Cheerio; chúng mạnh mẽ hơn nhưng không ngắn gọn như việc sử dụng một dòng lệnh.

Phân tích cú pháp HTML với Fish đều dựa vào việc ủy thác nhiệm vụ cho các công cụ chuyên biệt do khả năng thao tác văn bản hạn chế của nó. Fish gọi đến những công cụ này, thu thập đầu ra của chúng, và cho phép bạn thực hiện phép thuật lập trình của mình.

## Xem thêm
- [Pup GitHub Repo](https://github.com/ericchiang/pup) - Tài liệu và ví dụ.
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - Tìm hiểu thêm về Fish.
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) - Đối với việc phân tích cú pháp HTML phức tạp hơn trong Python.
- [Cheerio GitHub Repo](https://github.com/cheeriojs/cheerio) - Dành cho những ai quan tâm đến cách tiếp cận dựa trên JavaScript.

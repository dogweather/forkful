---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.009542-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML l\xE0 qu\xE1 tr\xECnh gi\u1EA3i m\xE3\
  \ lingua franca c\u1EE7a web \u0111\u1EC3 l\u1ECDc d\u1EEF li\u1EC7u ho\u1EB7c thao\
  \ t\xE1c n\u1ED9i dung. C\xE1c l\u1EADp tr\xECnh vi\xEAn ph\xE2n t\xEDch c\xFA ph\xE1\
  p HTML \u0111\u1EC3 t\u1EF1\u2026"
lastmod: 2024-02-19 22:04:56.441465
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML l\xE0 qu\xE1 tr\xECnh gi\u1EA3i m\xE3\
  \ lingua franca c\u1EE7a web \u0111\u1EC3 l\u1ECDc d\u1EEF li\u1EC7u ho\u1EB7c thao\
  \ t\xE1c n\u1ED9i dung. C\xE1c l\u1EADp tr\xECnh vi\xEAn ph\xE2n t\xEDch c\xFA ph\xE1\
  p HTML \u0111\u1EC3 t\u1EF1\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Phân tích cú pháp HTML là quá trình giải mã lingua franca của web để lọc dữ liệu hoặc thao tác nội dung. Các lập trình viên phân tích cú pháp HTML để tự động hóa việc thu thập dữ liệu web, tích hợp API, hoặc chuyển đổi định dạng dữ liệu.

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

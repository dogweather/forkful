---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.692441-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 ph\xE1 v\u1EE1\
  \ n\u1ED9i dung HTML \u0111\u1EC3 tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u c\u1EE5 th\u1EC3\
  . L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ t\u1EF1 \u0111\u1ED9ng h\xF3a vi\u1EC7c thu th\u1EADp d\u1EEF\u2026"
lastmod: 2024-02-19 22:04:56.126656
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 ph\xE1 v\u1EE1\
  \ n\u1ED9i dung HTML \u0111\u1EC3 tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u c\u1EE5 th\u1EC3\
  . L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ t\u1EF1 \u0111\u1ED9ng h\xF3a vi\u1EC7c thu th\u1EADp d\u1EEF\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Lý do & Mục đích?
Phân tích cú pháp HTML có nghĩa là phá vỡ nội dung HTML để trích xuất dữ liệu cụ thể. Lập trình viên thực hiện điều này để tự động hóa việc thu thập dữ liệu web, khai thác dữ liệu, hoặc để tích hợp nội dung web vào ứng dụng.

## Cách thực hiện:
Hãy lấy một số dữ liệu từ một trang web. Chúng ta sẽ sử dụng Invoke-WebRequest và sau đó lọc ra những gì chúng ta cần.

```PowerShell
# Tải nội dung trang
$response = Invoke-WebRequest -Uri "http://example.com"

# Phân tích cú pháp nội dung HTML
$parsedHtml = $response.ParsedHtml

# Trích xuất dữ liệu
# Giả sử chúng ta muốn tất cả các văn bản hyperlink
$links = $parsedHtml.getElementsByTagName('a') | ForEach-Object { $_.innerText }
$links
```

Kết quả mẫu:

```
Trang Chính
Về Chúng Tôi
Dịch Vụ
Liên Hệ
```

## Sâu hơn
Trong quá khứ, phân tích cú pháp HTML trong PowerShell có thể khá cồng kềnh. Bạn có lựa chọn sử dụng regex (nổi tiếng là có vấn đề đối với HTML), đối tượng COM với Internet Explorer, hoặc thư viện bên thứ ba. Nay, Cmdlet Invoke-WebRequest của PowerShell đơn giản hóa quá trình này, tích hợp với engine của Internet Explorer để phân tích cú pháp HTML - mặc dù nó hơi chậm và cồng kềnh.

Có các lựa chọn khác như thư viện HtmlAgilityPack, phù hợp hơn và được tinh chỉnh tốt hơn cho việc phân tích cú pháp HTML. Nó yêu cầu thiết lập thêm nhưng đem lại sự linh hoạt và hiệu suất.

Về việc triển khai, lưu ý rằng phương pháp của PowerShell không luôn chính xác đối với nội dung động được tải bởi JavaScript. Để xử lý nội dung động, bạn có thể cần đến các công cụ tự động hóa trình duyệt như Selenium.

## Xem Thêm
- [HtmlAgilityPack trên GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Selenium với PowerShell](https://github.com/adamdriscoll/selenium-powershell)

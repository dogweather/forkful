---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.692441-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y l\u1EA5y m\u1ED9t s\u1ED1 d\u1EEF\
  \ li\u1EC7u t\u1EEB m\u1ED9t trang web. Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng Invoke-WebRequest\
  \ v\xE0 sau \u0111\xF3 l\u1ECDc ra nh\u1EEFng g\xEC ch\xFAng ta c\u1EA7n."
lastmod: '2024-03-13T22:44:36.933342-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y l\u1EA5y m\u1ED9t s\u1ED1 d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t trang\
  \ web."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

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

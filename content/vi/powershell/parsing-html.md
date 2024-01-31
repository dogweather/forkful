---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:03:42.692441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

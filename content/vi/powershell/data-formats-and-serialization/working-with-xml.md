---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:33.796697-07:00
description: "L\xE0m th\u1EBF n\xE0o: K\u1EBFt qu\u1EA3 M\u1EABu."
lastmod: '2024-04-05T21:53:38.329530-06:00'
model: gpt-4-0125-preview
summary: ''
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Làm thế nào:
```PowerShell
# Tải một tệp XML vào một biến
[xml]$xmlContent = Get-Content 'đường\dẫn\đến\tệp.xml'

# Truy cập các nút XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Tiêu đề: $($book.title)"
}

# Tạo một phần tử XML mới
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Lưu lại XML vào tệp
$xmlContent.Save('đường\dẫn\đến\tệp\cập\nhật.xml')
```
Kết quả Mẫu:
```
Tiêu đề: Lập Trình PowerShell
Tiêu đề: Cơ Bản XML
```

## Sâu hơn nữa
XML, hoặc Ngôn ngữ Đánh dấu Mở rộng, đã tồn tại từ cuối những năm '90 và vẫn là một định dạng được sử dụng rộng rãi cho dữ liệu cấu trúc. PowerShell đơn giản hóa việc làm việc với XML so với các phương pháp phân tích truyền thống; nó chuyển đổi XML thành các đối tượng trực tiếp, cho phép bạn tương tác với các phần tử thông qua cú pháp dấu chấm quen thuộc.

Các phương án thay thế cho XML bao gồm JSON, YAML, hoặc các định dạng dữ liệu tùy chỉnh. JSON, ví dụ, đã trở nên phổ biến với bản chất nhẹ và dễ sử dụng với các công nghệ web. Tuy nhiên, các tính năng mở rộng của XML như không gian tên, lược đồ, và xử lý XSLT thường khiến nó phù hợp hơn với các tài liệu phức tạp hoặc tiêu chuẩn ngành.

PowerShell sử dụng khả năng XML của .NET Framework cho việc xử lý XML của mình. Điều này có nghĩa là không chỉ đơn giản là các thao tác đọc-viết; bạn cũng có thể làm việc với lược đồ XML để xác thực, sử dụng XPath cho các truy vấn và sử dụng các biến đổi XSLT, tất cả thông qua PowerShell.

## Xem Thêm
- [Hướng dẫn XML của W3Schools](https://www.w3schools.com/xml/)
- [XML so với JSON](https://www.json.org/json-en.html)

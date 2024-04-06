---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:33.796697-07:00
description: "L\xE0m th\u1EBF n\xE0o: XML, ho\u1EB7c Ng\xF4n ng\u1EEF \u0110\xE1nh\
  \ d\u1EA5u M\u1EDF r\u1ED9ng, \u0111\xE3 t\u1ED3n t\u1EA1i t\u1EEB cu\u1ED1i nh\u1EEF\
  ng n\u0103m '90 v\xE0 v\u1EABn l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng r\u1ED9ng r\xE3i cho d\u1EEF li\u1EC7u c\u1EA5u tr\xFA\
  c.\u2026"
lastmod: '2024-04-05T22:50:51.278713-06:00'
model: gpt-4-0125-preview
summary: "XML, ho\u1EB7c Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng,\
  \ \u0111\xE3 t\u1ED3n t\u1EA1i t\u1EEB cu\u1ED1i nh\u1EEFng n\u0103m '90 v\xE0 v\u1EAB\
  n l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng\
  \ r\u1ED9ng r\xE3i cho d\u1EEF li\u1EC7u c\u1EA5u tr\xFAc."
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

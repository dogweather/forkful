---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:33.796697-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi XML bao g\u1ED3m vi\u1EC7c thao t\xE1c v\xE0\
  \ truy c\u1EADp d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc trong Ng\xF4\
  n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng (eXtensible Markup Language).\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-02-25T18:49:35.314076-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML bao g\u1ED3m vi\u1EC7c thao t\xE1c v\xE0 truy\
  \ c\u1EADp d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc trong Ng\xF4n\
  \ ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng (eXtensible Markup Language).\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm việc với XML bao gồm việc thao tác và truy cập dữ liệu được cấu trúc trong Ngôn ngữ Đánh dấu Mở rộng (eXtensible Markup Language). Các lập trình viên làm việc với XML để bật khả năng tương tác với các hệ thống khác hoặc để đọc và viết các tệp cấu hình, nguồn dữ liệu, và các tài liệu cấu trúc khác thường gặp trong dịch vụ web.

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

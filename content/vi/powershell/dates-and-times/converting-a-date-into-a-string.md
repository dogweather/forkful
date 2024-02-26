---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:07.141714-07:00
description: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7\
  i trong PowerShell c\xF3 ngh\u0129a l\xE0 thay \u0111\u1ED5i \u0111\u1ED1i t\u01B0\
  \u1EE3ng `DateTime` th\xE0nh \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\u2026"
lastmod: '2024-02-25T18:49:35.297668-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7\
  i trong PowerShell c\xF3 ngh\u0129a l\xE0 thay \u0111\u1ED5i \u0111\u1ED1i t\u01B0\
  \u1EE3ng `DateTime` th\xE0nh \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc chuyển đổi một ngày thành chuỗi trong PowerShell có nghĩa là thay đổi đối tượng `DateTime` thành định dạng văn bản. Lập trình viên thực hiện điều này để định dạng ngày cho việc hiển thị, nhật ký, tên file, hoặc để tuần tự hóa thông tin cho việc lưu trữ và chuyển giao dữ liệu.

## Làm thế nào:

Để chuyển một ngày thành chuỗi, chúng ta sử dụng phương thức `ToString` hoặc toán tử định dạng `-f`. Dưới đây là cách thức:

```PowerShell
# Ngày và giờ hiện tại
$date = Get-Date

# Chuyển đổi mặc định thành chuỗi
$dateString = $date.ToString()
Write-Output $dateString

# Định dạng tùy chỉnh: Năm-Tháng-Ngày Giờ:Phút
$customFormat = $date.ToString("yyyy-MM-dd HH:mm")
Write-Output $customFormat

# Sử dụng toán tử -f cho định dạng tùy chỉnh giống như vậy
$fString = "{0:yyyy-MM-dd HH:mm}" -f $date
Write-Output $fString
```

Ví dụ đầu ra:

```
2023-03-17 10:45:00
2023-03-17 10:45
2023-03-17 10:45
```

## Sâu hơn

PowerShell, lấy cảm hứng từ các shell Unix và Windows Script Host, đã giới thiệu `Get-Date` trong giai đoạn phát triển đầu tiên vào khoảng năm 2006. Đây trở thành lệnh chính cho các thao tác ngày-giờ. Phương thức `ToString` trên các đối tượng `DateTime` và toán tử định dạng `-f` là những khái niệm được mượn từ .NET, mang lại hương vị hướng đối tượng cho PowerShell.

Nếu `ToString()` không được chỉ định với một định dạng, nó sẽ trả về ngày và giờ đầy đủ theo định dạng của văn hóa hiện tại. Nhưng khi bạn cần một bố cục cụ thể, như ISO 8601 hoặc chỉ ngày và tháng, các chuỗi định dạng ngày và giờ tùy chỉnh của .NET trở thành bạn đồng hành.

Còn một cách cũ nữa – sử dụng các mẫu định dạng của `DateTime` như `yyyy` cho năm bốn chữ số, `MM` cho tháng có đệm số không. Chúng trực quan và đa dạng để tạo ra bất kỳ định dạng ngày-giờ nào.

Sau đó là POSIX trong Unix, nơi các lệnh `date` thống trị với các chỉ dẫn định dạng của riêng họ. PowerShell đã kết nối hai thế giới này, áp dụng các phương thức quen thuộc nhưng cũng cung cấp khả năng tương thích nặng với hệ thống Windows.

Các phương án khác bao gồm việc nối cơ bản các thành phần ngày và sử dụng tiện ích bên ngoài hoặc cơ sở hạ tầng ngôn ngữ. Tuy nhiên, PowerShell ưa thích giữ mọi thứ trong nhà với các lệnh native mạnh mẽ.

Bạn có thể đi sâu hơn vào các chỉ dẫn định dạng trong tài liệu chính thức của Microsoft hoặc khám phá các blog do cộng đồng viết, thường chia sẻ các cách sáng tạo để thao tác với ngày và giờ trong PowerShell.

## Xem thêm

- Tài liệu chính thức của PowerShell về cmdlet [Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date), cung cấp cách sử dụng và ví dụ.
- Hướng dẫn về chuỗi định dạng tiêu chuẩn và tùy chỉnh của .NET [format strings guide](https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings) cho chi tiết định dạng sâu hơn.
- Các blog cộng đồng như diễn đàn [PowerShell.org](https://powershell.org/forums/) hoặc [Stack Overflow](https://stackoverflow.com/questions/tagged/powershell+datetime) để xem các ví dụ thực tế và thảo luận giải quyết vấn đề.

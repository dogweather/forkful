---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:07.141714-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 chuy\u1EC3n m\u1ED9t ng\xE0y th\xE0\
  nh chu\u1ED7i, ch\xFAng ta s\u1EED d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `ToString`\
  \ ho\u1EB7c to\xE1n t\u1EED \u0111\u1ECBnh d\u1EA1ng `-f`. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1ch th\u1EE9c."
lastmod: '2024-03-13T22:44:36.951419-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 chuy\u1EC3n m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i, ch\xFAng\
  \ ta s\u1EED d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `ToString` ho\u1EB7c to\xE1n t\u1EED\
  \ \u0111\u1ECBnh d\u1EA1ng `-f`."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

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

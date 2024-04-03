---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:03.726485-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PowerShell l\xE0m cho vi\u1EC7c ph\xE2\
  n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1ng tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3\
  n. H\xE3y xem c\xE1ch chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh \u0111\
  \u1ED1i t\u01B0\u1EE3ng DateTime."
lastmod: '2024-03-13T22:44:36.948875-06:00'
model: gpt-4-0125-preview
summary: "PowerShell l\xE0m cho vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1\
  ng tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Cách thực hiện:
PowerShell làm cho việc phân tích cú pháp ngày tháng trở nên khá đơn giản. Hãy xem cách chuyển đổi một chuỗi thành đối tượng DateTime.

```PowerShell
# Phân tích cú pháp cơ bản sử dụng ParseExact
$dateString = "March 31, 2023"
$format = "MMMM dd, yyyy"
$parsedDate = [datetime]::ParseExact($dateString, $format, $null)

# Đầu ra
$parsedDate
```

Điều này sẽ xuất ra:

```
Friday, March 31, 2023 12:00:00 AM
```

Đôi khi, chuỗi có các định dạng khác nhau. Hãy xử lý điều đó bằng `Parse` mặc định:

```PowerShell
# Phân tích cú pháp ngày với các định dạng khác nhau
$dateString = "2023-03-31T14:45:00"
$parsedDate = [datetime]::Parse($dateString)

# Đầu ra
$parsedDate
```

Đầu ra ở đây:

```
Friday, March 31, 2023 2:45:00 PM
```

Xử lý với các định dạng đặc biệt theo vùng? Sử dụng `ParseExact` với một vùng cụ thể:

```PowerShell
# Phân tích cú pháp theo vùng cụ thể
$dateString = "31/03/2023 16:45"
$format = "dd/MM/yyyy HH:mm"
$culture = [Globalization.CultureInfo]::GetCultureInfo("en-GB")
$parsedDate = [datetime]::ParseExact($dateString, $format, $culture)

# Đầu ra
$parsedDate
```

Sẽ xuất ra:

```
Friday, March 31, 2023 4:45:00 PM
```

## Xem xét kỹ lưỡng
Bây giờ, hãy nghiên cứu kỹ. Việc phân tích cú pháp ngày thường khó khăn do các định dạng ngày khác nhau trên toàn cầu. Ngày xưa, mỗi ngôn ngữ lập trình có cách riêng của mình để xử lý ngày tháng, thường dẫn đến sự không nhất quán. PowerShell, với tư cách là một ngôn ngữ kịch bản shell mới hơn, đã được lợi từ lớp DateTime của .NET, cung cấp các chức năng phân tích cú pháp mạnh mẽ.

Đối với các phương pháp thay thế, chúng ta có `Get-Date`, có thể phân tích cú pháp một chuỗi thành đối tượng DateTime nữa. Xem xét `TryParseExact` và `TryParse` nếu bạn mong đợi điều không mong đợi và muốn tránh ngoại lệ từ chuỗi không thể phân tích cú pháp.

Hãy nói về việc triển khai. Việc phân tích cú pháp ngày nhạy cảm với văn hóa, có nghĩa là định dạng của ngày có thể thay đổi theo khu vực. Đó là lý do tại sao chúng ta cung cấp thông tin về văn hóa và định dạng để xác định chính xác cấu trúc mong đợi của chuỗi ngày.

Đôi khi bạn sẽ gặp phải các định dạng rất kỳ lạ — hãy nhập phương pháp `ParseExact`, công cụ đa năng của bạn cho việc phân tích cú pháp ngày. Nó cho phép bạn chỉ định chính xác định dạng bạn đang mong đợi. Không còn phải đoán mò nữa.

## Xem Thêm
Để mở rộng kiến thức, nghiên cứu những tài liệu này:

- [Tài liệu chính thức của PowerShell về Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Tài liệu của .NET về DateTime.Parse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-6.0)
- [Xem xét về toàn cầu hóa và địa phương hóa](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)

Nhớ rằng, việc phân tích cú pháp ngày tháng là mạnh mẽ, vậy nên hãy sử dụng một cách khôn ngoan!

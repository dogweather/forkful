---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:46.649591-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u GET."
lastmod: '2024-03-13T22:44:36.932037-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u GET."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
Dưới đây là cách đơn giản để gửi một yêu cầu GET:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

Và, nếu bạn muốn POST một số thông tin:

```PowerShell
$body = @{
    'name' = 'Jane Doe'
    'occupation' = 'Space Ranger'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

Kết quả mẫu:

```
name         occupation
----         ----------
Jane Doe     Space Ranger
```

## Sâu hơn:
Việc gửi một yêu cầu HTTP quay lại với bình minh của việc phát triển web. Bạn đang tham gia vào một cuộc đối thoại với web bằng ngôn ngữ bản địa của nó, HTTP. Cmdlet `Invoke-RestMethod` của PowerShell là công cụ được chọn ở đây. Trước `Invoke-RestMethod`, `Invoke-WebRequest` là lựa chọn hàng đầu, và nó vẫn còn được sử dụng cho các phản hồi chi tiết hơn.

Nếu bạn muốn thử nghiệm, có những lựa chọn khác như `curl` hoặc lớp `HttpClient` của .NET. Khi sử dụng `Invoke-RestMethod`, hãy nhớ rằng nó là một bọc lấy cho các lớp và phương thức `HttpClient` của .NET, đem lại sự đơn giản nhưng đánh đổi một số kiểm soát ở cấp thấp.

Về mặt triển khai, hãy nhớ rằng các yêu cầu HTTP đi kèm với các phương thức như `GET`, `POST`, `PUT`, v.v. Tùy chỉnh tiêu đề với `-Headers`, và quản lý thời gian chờ và xác thực với các tham số bổ sung như cần thiết. Luôn làm sạch đầu vào nếu bạn sử dụng nội dung do người dùng tạo để tránh các cuộc tấn công chèn mã.

## Xem Thêm:
- [Giới thiệu về Invoke-RestMethod của PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Chi tiết về `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Hiểu về REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [Lớp `.NET HttpClient`](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)

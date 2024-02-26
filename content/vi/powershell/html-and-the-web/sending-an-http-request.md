---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:46.649591-07:00
description: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch b\u1EA1\
  n y\xEAu c\u1EA7u d\u1EEF li\u1EC7u ho\u1EB7c ph\u1EA3n h\u1ED3i t\u1EEB m\u1ED9\
  t d\u1ECBch v\u1EE5 web. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi c\xE1c API,\u2026"
lastmod: '2024-02-25T18:49:35.277541-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch b\u1EA1n\
  \ y\xEAu c\u1EA7u d\u1EEF li\u1EC7u ho\u1EB7c ph\u1EA3n h\u1ED3i t\u1EEB m\u1ED9\
  t d\u1ECBch v\u1EE5 web. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi c\xE1c API,\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Việc gửi một yêu cầu HTTP là cách bạn yêu cầu dữ liệu hoặc phản hồi từ một dịch vụ web. Các lập trình viên thực hiện điều này để tương tác với các API, lấy nội dung trang web, hoặc giao tiếp với các máy chủ từ xa.

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

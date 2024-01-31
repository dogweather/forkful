---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:46.649591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

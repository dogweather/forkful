---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:41.830339-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch b\u1EA1n l\u1ECBch s\u1EF1 y\xEAu c\u1EA7u m\xE1y ch\u1EE7 d\u1EEF li\u1EC7u\
  \ v\u1EDBi m\u1ED9t 'l\u1EDDi xin ph\xE9p' d\u01B0\u1EDBi d\u1EA1ng x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.935936-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n l\u1ECBch s\u1EF1 y\xEA\
  u c\u1EA7u m\xE1y ch\u1EE7 d\u1EEF li\u1EC7u v\u1EDBi m\u1ED9t 'l\u1EDDi xin ph\xE9\
  p' d\u01B0\u1EDBi d\u1EA1ng x\xE1c th\u1EF1c c\u01A1 b\u1EA3n."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách thực hiện:
Dưới đây là cách bạn lịch sự yêu cầu máy chủ dữ liệu với một 'lời xin phép' dưới dạng xác thực cơ bản:

```PowerShell
# Chuẩn bị thông tin đăng nhập
$user = 'TênĐăngNhậpCủaBạn'
$pass = 'MậtKhẩuCủaBạn'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# Thiết lập các headers
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# URL bạn đang gõ cửa
$url = 'https://api.example.com/data'

# Bây giờ, hãy thực hiện cuộc gọi
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# Xuất kết quả
$response
```

Đầu ra mẫu có thể trông như thế này, giả sử rằng phản hồi ở định dạng JSON:

```json
{
    "name": "John Doe",
    "email": "john@example.com"
}
```

## Đào sâu
Xác thực cơ bản là phương pháp cũ, bắt nguồn từ những ngày đầu của internet khi mọi người biết mọi người. Mặc dù vẫn được sử dụng, nó không an toàn lắm khi tự nó - giống như gửi mật khẩu bí mật của bạn trên một tấm bưu thiếp. Ngày nay, chúng ta thường gửi nó qua HTTPS để mã hóa nó, giống như đặt tấm bưu thiếp đó trong một chiếc hộp có khóa.

Các phương án thay thế? Rất nhiều. Bạn có API keys, OAuth, bearer tokens... danh sách còn dài. Mỗi phương án đều có cách bắt tay và mật khẩu bí mật riêng.

Về mặt thực hiện, với PowerShell, bạn đang chuyển tên đăng nhập và mật khẩu của mình thành một định dạng mà giao thức HTTP có thể hiểu được – base64. Nhưng nhớ rằng, base64 không phải là mã hóa; đó chỉ là việc trang điểm cho văn bản. Bất kỳ ai theo dõi cũng có thể khám phá ra nó trừ khi nó được gửi qua HTTPS.

## Xem Thêm
- [Tài liệu về Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Xác thực Truy cập Cơ bản HTTP trên MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Hiểu về Mã hóa Base64](https://en.wikipedia.org/wiki/Base64)
- [Thông tin về Mã hóa HTTPS](https://en.wikipedia.org/wiki/HTTPS)

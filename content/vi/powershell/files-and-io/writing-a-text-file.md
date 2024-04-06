---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:02.492873-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\
  \u1EC3 vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong PowerShell; n\xF3\
  \ c\u1EF1c k\u1EF3 d\u1EC5 d\xE0ng! T\u1EA1o v\xE0 vi\u1EBFt v\u0103n b\u1EA3n v\xE0\
  o m\u1ED9t t\u1EC7p m\u1EDBi."
lastmod: '2024-04-05T22:37:45.672437-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 vi\u1EBFt v\xE0o m\u1ED9\
  t t\u1EC7p v\u0103n b\u1EA3n trong PowerShell; n\xF3 c\u1EF1c k\u1EF3 d\u1EC5 d\xE0\
  ng!."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Dưới đây là cách để viết vào một tệp văn bản trong PowerShell; nó cực kỳ dễ dàng!

Tạo và viết văn bản vào một tệp mới:
```PowerShell
"Hello, world!" | Out-File -FilePath .\hello.txt
```

Thêm văn bản vào một tệp đã tồn tại:
```PowerShell
"Welcome to PowerShell scripting!" | Add-Content -Path .\hello.txt
```

Kiểm tra nội dung của tệp:
```PowerShell
Get-Content .\hello.txt
```

Kết quả mẫu:
```
Hello, world!
Welcome to PowerShell scripting!
```

## Đào Sâu
Các tệp PowerShell mặc định sử dụng mã hóa UTF-16. Trong lịch sử, các tệp văn bản đơn giản hơn—chỉ dùng ASCII. Bây giờ, `Out-File` và `Add-Content` cho phép bạn chọn mã hóa. Nếu bạn theo phong cách cũ, `Set-Content` vẫn tồn tại nhưng có những hạn chế. Đối với các tệp lớn hơn, bạn nên xem xét `[System.IO.StreamWriter]` để tăng hiệu quả.

## Xem Thêm
Để biết thêm về kỹ năng xử lý tệp trong PowerShell, hãy truy cập:
- Microsoft Docs về [Out-File](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- Microsoft Docs về [Add-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content)
  
Nhớ rằng, sự thực hành làm nên sự hoàn hảo. Vậy nên bắt đầu viết kịch bản thôi!

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:02.492873-07:00
description: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n t\xFA\
  y tr\xEAn \u0111\u0129a. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y cho m\u1EE5c \u0111\xEDch ghi nh\u1EADt k\xFD, c\u1EA5u h\xECnh, ho\u1EB7\
  c\u2026"
lastmod: '2024-03-13T22:44:36.960332-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 l\u01B0u tr\u1EEF d\u1EEF\
  \ li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n t\xFAy tr\xEA\
  n \u0111\u0129a."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Cái gì & Tại sao?
Viết một tệp văn bản là lưu trữ dữ liệu dưới dạng văn bản thuần túy trên đĩa. Lập trình viên thực hiện việc này cho mục đích ghi nhật ký, cấu hình, hoặc lưu trữ dữ liệu người dùng. Đó là nền tảng nhưng cực kỳ quan trọng cho hầu hết các ứng dụng.

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

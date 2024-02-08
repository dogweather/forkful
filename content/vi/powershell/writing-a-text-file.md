---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:13:02.492873-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

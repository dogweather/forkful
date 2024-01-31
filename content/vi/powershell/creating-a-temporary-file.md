---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:51.470946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tạo một tệp tạm thời có nghĩa là tạo một tệp để sử dụng trong thời gian ngắn, thường là để lưu trữ dữ liệu trong suốt một phiên làm việc. Các lập trình viên thực hiện việc này để tránh làm rối loạn hệ thống và để xử lý dữ liệu không cần được lưu trữ lâu dài.

## Cách thực hiện:
Để tạo ra một tệp tạm thời trong PowerShell, bạn sử dụng `New-TemporaryFile`. Cmdlet này tạo một tệp tạm thời trong thư mục temp của bạn. Dưới đây là bí kíp:

```PowerShell
$tempFile = New-TemporaryFile
```

Dòng này triệu hồi một tệp tạm thời mới từ không gian số. Bạn muốn biết nó nằm ở đâu? Chỉ cần gõ:

```PowerShell
$tempFile.FullName
```

Và bam! Nó sẽ cho bạn biết đường dẫn của tệp. Khi bạn đã xong và muốn dọn dẹp, chỉ cần xóa bỏ nó:

```PowerShell
Remove-Item $tempFile.FullName
```

Tệp biến mất, không để lại dấu vết.

## Sâu hơn nữa
Bây giờ, chúng ta hãy đi sâu vào vấn đề. Lịch sử, tệp tạm thời đã được sử dụng từ bình minh của việc tính toán, chủ yếu bởi vì RAM thì khan hiếm và đắt đỏ. Những tệp giao dịch này là một giải pháp tạm thời cho bộ nhớ hạn chế.

Khi nói đến các phương án thay thế, một số nhà phát triển tự tạo đường dẫn tệp tạm thời của họ sử dụng `[System.IO.Path]::GetTempFileName()`, hoạt động trên các ngôn ngữ được hỗ trợ bởi .NET khác nhau và mang lại cho bạn nhiều quyền kiểm soát hơn.

Trong PowerShell, `New-TemporaryFile` thực sự là một bao bọc gọn gàng xung quanh phương thức .NET này. Nó tạo ra một tệp ở một đường dẫn như `C:\Users\TenBan\AppData\Local\Temp\tmpXXXX.tmp` (`XXXX` là một số ngẫu nhiên). Phần mở rộng `.tmp` là một quy ước, biểu thị tính tạm thời.

Nhớ rằng, tệp tạm thời nên được xử lí một cách thích hợp. Nếu bạn tạo ra nhiều tệp hoặc xử lý dữ liệu nhạy cảm, bạn nên xóa sạch chúng một cách an toàn để tránh rò rỉ dữ liệu.

## Xem thêm
- Để biết thêm về `New-TemporaryFile`, hãy kiểm tra [tài liệu](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile).
- Khám phá các phương thức của lớp `System.IO.Path` trên [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-6.0).

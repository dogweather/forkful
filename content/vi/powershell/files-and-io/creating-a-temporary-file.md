---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:51.470946-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 t\u1EA1o ra m\u1ED9t t\u1EC7\
  p t\u1EA1m th\u1EDDi trong PowerShell, b\u1EA1n s\u1EED d\u1EE5ng `New-TemporaryFile`.\
  \ Cmdlet n\xE0y t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi trong th\u01B0 m\u1EE5\
  c temp c\u1EE7a b\u1EA1n.\u2026"
lastmod: '2024-03-13T22:44:36.961604-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 t\u1EA1o ra m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi trong PowerShell,\
  \ b\u1EA1n s\u1EED d\u1EE5ng `New-TemporaryFile`."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

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

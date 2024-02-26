---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:33.506876-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ k\xE9o n\u1ED9i dung c\u1EE7a n\xF3 v\xE0o m\u1ED9t h\xECnh th\u1EE9c m\xE0 ch\u01B0\
  \u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y\u2026"
lastmod: '2024-02-25T18:49:35.305632-07:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ k\xE9o n\u1ED9i dung c\u1EE7a n\xF3 v\xE0o m\u1ED9t h\xECnh th\u1EE9c m\xE0 ch\u01B0\
  \u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Đọc một tệp văn bản có nghĩa là kéo nội dung của nó vào một hình thức mà chương trình của bạn có thể làm việc với. Các lập trình viên thực hiện việc này để xử lý dữ liệu, cấu hình, ghi nhật ký – bất cứ khi nào một tệp có thể đọc được bởi con người được liên quan.

## Làm thế nào:
Hãy bắt đầu với những điều cơ bản! Dưới đây là cách bạn đọc từ một tệp văn bản trong PowerShell:

```PowerShell
# Lấy nội dung của một tệp
$content = Get-Content -Path "C:\duongdan\den\tap\xin_ban.txt"
# Hiển thị nội dung trong bảng điều khiển
Write-Output $content
```

Đầu ra mẫu có thể trông như thế này nếu tệp của bạn chứa vài dòng văn bản:
```
Xin chào, PowerShell!
Kết thúc tệp.
```

Bây giờ, bạn muốn đọc từng dòng một?

```PowerShell
# Đọc tệp từng dòng một
$lines = Get-Content -Path "C:\duongdan\den\tap\xin_ban.txt" -ReadCount 0
foreach ($line in $lines) {
    Write-Output $line
}
```

Đầu ra mẫu giống như trên, nhưng được xử lý từng dòng một.

## Tìm hiểu sâu
Lâu trước khi có PowerShell, các công cụ dòng lệnh như `cat` trong các hệ thống giống UNIX hoặc `type` trong DOS là cách thức đầu tiên để đọc tệp. Get-Content của PowerShell là công cụ sắc bén cho việc này ngày nay, với những lợi ích bổ sung như đọc từng dòng, giúp tránh quá tải bộ nhớ với các tệp lớn.

Ngoài `Get-Content`, chúng ta còn có các lớp `.NET` để kiểm soát nhiều hơn - nhập `System.IO.StreamReader`:

```PowerShell
$stream = [System.IO.StreamReader] "C:\duongdan\den\tap\xin_ban.txt"
try {
    while ($line = $stream.ReadLine()) {
        Write-Output $line
    }
}
finally {
    $stream.Close()
}
```

Đây là một phương thức hiệu quả về bộ nhớ, hỗ trợ cho những bản văn khổng lồ.

Có lựa chọn khác không? Có, bạn có thể sử dụng `Import-Csv` cho các tệp CSV hoặc `ConvertFrom-Json` cho JSON, nếu bạn muốn chuyển dữ liệu vào các đối tượng có cấu trúc. Nhưng hãy gắn bó với `Get-Content` cho những thứ văn bản thô.

## Xem thêm
Truy cập tài liệu chính thức để biết thêm thông tin:

- [Tài liệu Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [Về Biến Tự động](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables) - Điều này cung cấp cái nhìn sâu sắc về các biến như `$_`, có thể hữu ích cho quá trình xử lý ngắn gọn.
- [Sử dụng khả năng .NET của PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/developer/hosting/adding-and-invoking-commands?view=powershell-7.1) - Dành cho những người muốn khám phá sâu hơn vào khung .NET trong PowerShell.

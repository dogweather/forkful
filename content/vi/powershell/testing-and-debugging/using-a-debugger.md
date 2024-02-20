---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:29.561831-07:00
description: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp \u0111i\u1EC3m d\u1EEBng, b\u01B0\u1EDBc qua m\xE3 c\u1EE7\
  a b\u1EA1n, theo d\xF5i c\xE1c bi\u1EBFn, v\xE0 ki\u1EC3m tra tr\u1EA1ng th\xE1\
  i c\u1EE7a ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n khi n\xF3\u2026"
lastmod: 2024-02-19 22:04:56.136393
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i c\xF3 ngh\u0129a l\xE0\
  \ thi\u1EBFt l\u1EADp \u0111i\u1EC3m d\u1EEBng, b\u01B0\u1EDBc qua m\xE3 c\u1EE7\
  a b\u1EA1n, theo d\xF5i c\xE1c bi\u1EBFn, v\xE0 ki\u1EC3m tra tr\u1EA1ng th\xE1\
  i c\u1EE7a ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n khi n\xF3\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Sử dụng một trình gỡ lỗi có nghĩa là thiết lập điểm dừng, bước qua mã của bạn, theo dõi các biến, và kiểm tra trạng thái của chương trình của bạn khi nó chạy. Đó là một sự thay đổi lớn đối với lập trình viên bởi vì nó xác định lỗi và giúp chúng ta hiểu rõ hơn về những gì mã của chúng ta thực sự làm.

## Làm thế nào:
Trong PowerShell, bạn có thể gỡ lỗi script sử dụng PowerShell Integrated Scripting Environment (ISE) tích hợp sẵn hoặc Visual Studio Code (VS Code) với tiện ích mở rộng PowerShell. Dưới đây là cách sử dụng điểm dừng trong cả hai:

### PowerShell ISE:
```PowerShell
# Đặt một điểm dừng ở một dòng cụ thể
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Chạy script của bạn một cách bình thường
.\MyScript.ps1

# Khi script chạm vào điểm dừng, bạn có thể kiểm tra các biến
$myVariable

# Tiếp tục thực thi
Continue
```

### Visual Studio Code:
```PowerShell
# Mở script PowerShell của bạn trong VS Code.
# Nhấp vào bên trái của số dòng để đặt một điểm dừng.
# Bắt đầu gỡ lỗi bằng cách nhấn F5 hoặc nhấp vào 'Bắt đầu Gỡ Lỗi'.

# VS Code sẽ dừng thực thi ở điểm dừng của bạn.
# Sử dụng bảng gỡ lỗi để theo dõi biến, kiểm tra call stack, và điều khiển luồng.
```

Gỡ lỗi trong cả hai môi trường cho phép bạn nhảy vào (F11), nhảy qua (F10), và nhảy ra (Shift+F11) trong khi gỡ lỗi.

## Đi sâu hơn
Trước đây, gỡ lỗi trong PowerShell có chút khó khăn; nó đòi hỏi nhiều dòng `Write-Host` để xuất trạng thái của biến hoặc phương pháp thử và sai cổ điển. Với sự xuất hiện của PowerShell ISE, và gần đây hơn, VS Code với các tính năng gỡ lỗi phong phú của nó, việc gỡ lỗi trong PowerShell trở nên gần như trực quan như trong các ngôn ngữ lập trình đầy đủ.

Các phương án thay thế cho các công cụ gỡ lỗi bản địa của PowerShell bao gồm các công cụ của bên thứ ba như PowerGUI hoặc sử dụng các IDE mạnh mẽ như Visual Studio với plugin PowerShell.

Khi triển khai một trình gỡ lỗi, cân nhắc về phạm vi script, đặc biệt khi làm việc với các script hoặc module đã được nạp bằng dấu chấm. Các điểm dừng có thể dựa trên điều kiện, thay đổi biến, hoặc dựa trên dòng, cho phép kiểm soát chính xác trong một phiên gỡ lỗi.

Hơn nữa, với sự chuyển đổi sang PowerShell Core (PowerShell đa nền tảng), việc gỡ lỗi phần lớn đã chuyển sang tay của VS Code, cung cấp một trải nghiệm nhất quán trên các nền tảng khác nhau.

## Xem Thêm
Để biết thêm về gỡ lỗi trong PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:29.561831-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong PowerShell, b\u1EA1n c\xF3 th\u1EC3 g\u1EE1\
  \ l\u1ED7i script s\u1EED d\u1EE5ng PowerShell Integrated Scripting Environment\
  \ (ISE) t\xEDch h\u1EE3p s\u1EB5n ho\u1EB7c Visual Studio Code (VS\u2026"
lastmod: '2024-03-13T22:44:36.942450-06:00'
model: gpt-4-0125-preview
summary: "Trong PowerShell, b\u1EA1n c\xF3 th\u1EC3 g\u1EE1 l\u1ED7i script s\u1EED\
  \ d\u1EE5ng PowerShell Integrated Scripting Environment (ISE) t\xEDch h\u1EE3p s\u1EB5\
  n ho\u1EB7c Visual Studio Code (VS Code) v\u1EDBi ti\u1EC7n \xEDch m\u1EDF r\u1ED9\
  ng PowerShell."
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

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

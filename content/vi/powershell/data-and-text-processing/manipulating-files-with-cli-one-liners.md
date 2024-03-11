---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:53.471613-07:00
description: "Vi\u1EC7c thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c\
  \ l\u1EC7nh m\u1ED9t d\xF2ng trong PowerShell li\xEAn quan \u0111\u1EBFn vi\u1EC7\
  c nhanh ch\xF3ng thay \u0111\u1ED5i, di chuy\u1EC3n ho\u1EB7c l\u1EA5y d\u1EEF li\u1EC7\
  u t\u1EC7p tin tr\u1EF1c\u2026"
lastmod: '2024-03-11T00:14:10.222529-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c l\u1EC7\
  nh m\u1ED9t d\xF2ng trong PowerShell li\xEAn quan \u0111\u1EBFn vi\u1EC7c nhanh\
  \ ch\xF3ng thay \u0111\u1ED5i, di chuy\u1EC3n ho\u1EB7c l\u1EA5y d\u1EEF li\u1EC7\
  u t\u1EC7p tin tr\u1EF1c\u2026"
title: "Thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c l\u1EC7nh CLI ch\u1EC9\
  \ m\u1ED9t d\xF2ng"
---

{{< edit_this_page >}}

## Điều gì & Tại sao?

Việc thao tác với các tệp tin bằng các lệnh một dòng trong PowerShell liên quan đến việc nhanh chóng thay đổi, di chuyển hoặc lấy dữ liệu tệp tin trực tiếp từ dòng lệnh. Lập trình viên làm điều này vì hiệu quả; nó nhanh hơn là dùng giao diện người dùng đồ họa (GUI) hoặc viết các kịch bản dài cho các tác vụ đơn giản.

## Làm thế nào:

### Đọc một Tệp
Để nhanh chóng hiển thị nội dung của một tệp, sử dụng lệnh `Get-Content`:
```PowerShell
Get-Content .\example.txt
```

### Viết vào Tệp
Để viết điều gì đó mới vào một tệp, có thể sử dụng `Set-Content`:
```PowerShell
Set-Content -Path .\example.txt -Value "Xin chào, PowerShell!"
```

### Thêm vào Tệp
Thêm dữ liệu vào cuối một tệp mà không xóa nội dung của nó có thể được thực hiện với `Add-Content`:
```PowerShell
Add-Content -Path .\example.txt -Value "Thêm dòng này."
```

### Sao chép Tệp
Sao chép một tệp được thực hiện dễ dàng với `Copy-Item`:
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### Xóa Tệp
Để xóa một tệp, chỉ cần sử dụng `Remove-Item`:
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### Tìm kiếm Trong Tệp
Sử dụng `Select-String` để tìm kiếm văn bản trong tệp:
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### Kết hợp các Lệnh
PowerShell thật sự tỏa sáng với khả năng kết hợp các lệnh thông qua ống. Đây là cách bạn có thể tìm tệp và sao chép chúng vào một thư mục mới:
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## Sâu hơn

Lịch sử, PowerShell được giới thiệu như một phương thức mạnh mẽ hơn so với dấu nhắc lệnh truyền thống trong Windows, cung cấp quyền truy cập chưa từng có vào nội bộ hệ thống và cửa hàng dữ liệu. Nó kết hợp tốc độ của dòng lệnh với sự linh hoạt của việc kịch bản hóa, khiến nó trở thành công cụ không thể thiếu cho các quản trị hệ thống và nhà phát triển dựa trên Windows.

Các phương pháp thay thế cho PowerShell trong việc thao tác tệp bao gồm các công cụ dựa trên Unix như `sed`, `awk`, `grep` và kịch bản `bash` cho người dùng Linux và MacOS. Mặc dù những công cụ này cực kỳ mạnh mẽ và có những ưu điểm riêng, PowerShell cung cấp sự tích hợp sâu rộng với môi trường Windows.

Một khía cạnh đáng chú ý của PowerShell là bản chất hướng đối tượng của nó. Không giống như nhiều ngôn ngữ kịch bản khác xem mọi thứ như là chuỗi hay dòng byte, PowerShell làm việc trực tiếp với các đối tượng .NET. Điều này có nghĩa là khi bạn thao tác với tệp, bạn đang làm việc với các đối tượng phong phú cung cấp nhiều tính năng và phương thức, làm cho các tác vụ phức tạp trở nên dễ quản lý hơn.

Một trong những điểm yếu của PowerShell, đặc biệt đối với người dùng Linux và MacOS, là tính dài dòng so với việc viết kịch bản bash hoặc sử dụng các công cụ dòng lệnh Unix. Bên cạnh đó, sự tích hợp sâu rộng của PowerShell với Windows đôi khi có thể khiến các kịch bản đa nền tảng trở nên phức tạp hơn một chút, mặc dù những nỗ lực với PowerShell Core đang nỗ lực khắc phục hiệu quả khoảng cách đó.

Bất chấp những điểm yếu, sức mạnh của PowerShell nằm ở khả năng mạnh mẽ của nó với các lệnh một dòng, môi trường kịch bản tích hợp, và quyền truy cập toàn diện nó cung cấp đến hệ sinh thái Windows, tạo thành một công cụ thiết yếu cho những ai muốn thao tác tệp và nhiều hơn nữa trực tiếp từ dòng lệnh.

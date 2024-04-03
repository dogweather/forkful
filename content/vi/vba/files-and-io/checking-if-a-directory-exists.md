---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:58.105441-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong VBA, \u0111\u1EC3 ki\u1EC3m tra xem m\u1ED9\
  t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng, b\u1EA1n th\u01B0\u1EDDng\
  \ s\u1EED d\u1EE5ng h\xE0m `Dir` k\u1EBFt h\u1EE3p v\u1EDBi thu\u1ED9c t\xEDnh `vbDirectory`.\
  \ Ph\u01B0\u01A1ng ph\xE1p n\xE0y\u2026"
lastmod: '2024-03-13T22:44:36.452260-06:00'
model: gpt-4-0125-preview
summary: "Trong VBA, \u0111\u1EC3 ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3\
  \ t\u1ED3n t\u1EA1i kh\xF4ng, b\u1EA1n th\u01B0\u1EDDng s\u1EED d\u1EE5ng h\xE0\
  m `Dir` k\u1EBFt h\u1EE3p v\u1EDBi thu\u1ED9c t\xEDnh `vbDirectory`."
title: "Ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3n t\u1EA1i"
weight: 20
---

## Làm thế nào:
Trong VBA, để kiểm tra xem một thư mục có tồn tại không, bạn thường sử dụng hàm `Dir` kết hợp với thuộc tính `vbDirectory`. Phương pháp này cho phép bạn kiểm tra sự tồn tại của một thư mục bằng cách chỉ rõ đường dẫn của nó. Đây là cách bạn có thể làm:

```basic
Dim folderPath As String
folderPath = "C:\TestFolder"

If Dir(folderPath, vbDirectory) = "" Then
    MsgBox "Thư mục không tồn tại.", vbExclamation
Else
    MsgBox "Thư mục tồn tại.", vbInformation
End If
```

Đoạn mã trên đầu tiên định nghĩa một đường dẫn thư mục (`C:\TestFolder`). Hàm `Dir` sau đó cố gắng tìm thư mục này sử dụng thuộc tính `vbDirectory`. Nếu thư mục không tồn tại, `Dir` sẽ trả về một chuỗi trống, và chúng ta hiển thị một hộp thông báo cho biết thư mục không tồn tại. Ngược lại, chúng ta hiển thị một thông báo khác cho biết thư mục tồn tại.

Kết quả mẫu khi thư mục không tồn tại:
```
Thư mục không tồn tại.
```

Kết quả mẫu khi thư mục tồn tại:
```
Thư mục tồn tại.
```

## Sâu hơn nữa
Việc kiểm tra xem một thư mục có tồn tại không là một nhiệm vụ cơ bản trong nhiều ngôn ngữ lập trình, không chỉ trong VBA. Phương pháp sử dụng `Dir` được mô tả ở trên là đơn giản và hiệu quả cho hầu hết các mục đích trong VBA. Tuy nhiên, cần lưu ý rằng cách tiếp cận này có thể có hạn chế, chẳng hạn như trong trường hợp của đường dẫn mạng và xử lý quyền, có thể đôi khi mang lại kết quả sai lệch.

Theo lịch sử, các phương pháp truy cập hệ thống tập tin đã phát triển qua các ngôn ngữ lập trình khác nhau, với những ngôn ngữ gần đây hơn cung cấp các cách tiếp cận hướng đối tượng. Ví dụ, trong các ngôn ngữ .NET như VB.NET, người ta có thể sử dụng `System.IO.Directory.Exists(path)` cho một cách thức kiểm tra tồn tại thư mục dễ dàng và có thể nói là mạnh mẽ hơn, tận dụng việc xử lý ngoại lệ và thông tin trả về phong phú hơn.

Mặc dù VBA không có các lớp tích hợp sẵn mạnh mẽ như những cái được tìm thấy trong .NET cho các thao tác hệ thống tập tin, nhưng hiểu được tiện ích và hạn chế của hàm `Dir` là quan trọng để viết các kịch bản VBA hiệu quả tương tác với hệ thống tập tin. Trong các tình huống mà khả năng của VBA không đủ, việc tích hợp các thành phần .NET hoặc tận dụng các kịch bản bên ngoài có thể cung cấp những lựa chọn tốt hơn.

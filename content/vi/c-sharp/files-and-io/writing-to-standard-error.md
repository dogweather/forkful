---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:28.844264-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C#, vi\u1EBFt v\xE0o stderr b\u1EB1ng\
  \ c\xE1ch s\u1EED d\u1EE5ng `Console.Error.WriteLine()`. N\xF3 t\u01B0\u01A1ng t\u1EF1\
  \ nh\u01B0 `Console.WriteLine()`, ch\u1EC9 l\xE0 nh\u1EAFm v\xE0o lu\u1ED3ng l\u1ED7\
  i."
lastmod: '2024-03-13T22:44:36.679030-06:00'
model: gpt-4-0125-preview
summary: "Trong C#, vi\u1EBFt v\xE0o stderr b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng `Console.Error.WriteLine()`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Làm thế nào:
Trong C#, viết vào stderr bằng cách sử dụng `Console.Error.WriteLine()`. Nó tương tự như `Console.WriteLine()`, chỉ là nhắm vào luồng lỗi.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Thông điệp Đầu ra Chuẩn."); // Đi tới stdout
        Console.Error.WriteLine("Thông điệp Lỗi!"); // Đi tới stderr
    }
}
```

Kết quả mẫu khi mọi thứ đều ổn:

```
Thông điệp Đầu ra Chuẩn.
```

Nhưng, nếu có gì đó không ổn, bạn sẽ thấy:

```
Thông điệp Đầu ra Chuẩn.
Thông điệp Lỗi!
```

Thông điệp lỗi xuất hiện trong bảng điều khiển hoặc có thể được chuyển hướng sang một tệp.

## Đào Sâu
Về mặt lịch sử, việc phân tách stdout và stderr bắt nguồn từ các hệ thống Unix, nơi nó cho phép xử lý dữ liệu sạch sẽ và xử lý lỗi. Trong C# (và .NET nói chung), `Console.Out` đại diện cho stdout, trong khi `Console.Error` đại diện cho stderr.

Bạn có thể chuyển hướng cả hai bằng cách sử dụng `Console.SetOut()` và `Console.SetError()`. Các luồng như `FileStream` hoặc `StringWriter` có thể bắt lại đầu ra cho việc ghi lại. Điều này rất quan trọng trong các tình huống mà thông điệp lỗi không nên lẫn lộn với dữ liệu bình thường, chẳng hạn, khi stdout được ống sang một chương trình khác.

## Xem Thêm
- [Thuộc tính Console.Error - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [Lớp Stream .NET - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.stream)

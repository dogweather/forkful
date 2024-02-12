---
title:                "Ghi vào lỗi chuẩn"
aliases: - /vi/c-sharp/writing-to-standard-error.md
date:                  2024-01-28T22:13:28.844264-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Việc gửi các thông báo lỗi tới standard error (stderr) có nghĩa là bạn đang chuyển thông điệp lỗi của mình ra khỏi đầu ra thông thường (stdout). Các lập trình viên làm điều này để phân tách dữ liệu bình thường ra khỏi thông tin lỗi, điều này giúp trong việc ghi lại và gỡ lỗi.

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

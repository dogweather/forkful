---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:47.562124-07:00
description: "L\xE0m th\u1EBF n\xE0o: B\u1EA1n c\xF3 th\u1EC3 vi\u1EBFt m\u1ED9t t\u1EC7\
  p v\u0103n b\u1EA3n trong C# b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng `File.WriteAllText`,\
  \ `File.AppendAllText`, ho\u1EB7c m\u1ED9t `StreamWriter`."
lastmod: '2024-03-13T22:44:36.681630-06:00'
model: gpt-4-0125-preview
summary: "B\u1EA1n c\xF3 th\u1EC3 vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong\
  \ C# b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng `File.WriteAllText`, `File.AppendAllText`,\
  \ ho\u1EB7c m\u1ED9t `StreamWriter`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Bạn có thể viết một tệp văn bản trong C# bằng cách sử dụng `File.WriteAllText`, `File.AppendAllText`, hoặc một `StreamWriter`.

```c#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Viết văn bản vào một tệp mới
        File.WriteAllText("log.txt", "Xin chào, tệp!");

        // Thêm văn bản vào tệp hiện tại
        File.AppendAllText("log.txt", "\nHãy thêm một dòng nữa.");

        // Sử dụng StreamWriter để viết vào tệp
        using (StreamWriter writer = new StreamWriter("log.txt", true))
        {
            writer.WriteLine("Một dòng nữa với StreamWriter.");
        }
    }
}
```

Kết quả mẫu trong `log.txt`:
```
Xin chào, tệp!
Hãy thêm một dòng nữa.
Một dòng nữa với StreamWriter.
```

## Tìm hiểu sâu
Theo lịch sử, I/O tệp trong C# đã phát triển từ các hoạt động `FileStream` cơ bản đến các trừu tượng như `StreamWriter`. Các phương án thay thế bao gồm sử dụng `System.IO.FileStream` để kiểm soát nhiều hơn hoặc các phương pháp không đồng bộ như `WriteAllTextAsync` để tăng hiệu quả. Bên dưới, `StreamWriter` sử dụng một bộ đệm để tối ưu hóa các hoạt động viết.

## Xem thêm
Để đọc thêm và hướng dẫn chi tiết:
- [Tài liệu MSDN về I/O Tệp](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [Lớp StreamWriter của MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [Hướng dẫn về I/O Tệp không đồng bộ trong C#](https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o)

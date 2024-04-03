---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:47.562124-07:00
description: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u nh\u01B0 chu\u1ED7i v\xE0o m\u1ED9t t\u1EC7\
  p tr\xEAn \u0111\u0129a c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ghi l\u1EA1i nh\u1EADt k\xFD, l\u01B0u c\u1EA5\
  u\u2026"
lastmod: '2024-03-13T22:44:36.681630-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0 l\u01B0\
  u tr\u1EEF d\u1EEF li\u1EC7u nh\u01B0 chu\u1ED7i v\xE0o m\u1ED9t t\u1EC7p tr\xEA\
  n \u0111\u0129a c\u1EE7a b\u1EA1n."
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

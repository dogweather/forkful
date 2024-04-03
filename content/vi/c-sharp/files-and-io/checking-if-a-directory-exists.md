---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:20.947915-07:00
description: "C\xE1ch l\xE0m: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3\
  \ th\u1EC3 ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i\
  \ kh\xF4ng s\u1EED d\u1EE5ng namespace `System.IO`."
lastmod: '2024-03-13T22:44:36.676473-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3 th\u1EC3 ki\u1EC3\
  m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng s\u1EED d\u1EE5\
  ng namespace `System.IO`."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cách làm:
Dưới đây là cách bạn có thể kiểm tra xem một thư mục có tồn tại không sử dụng namespace `System.IO`:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\exampleFolder";

        if (Directory.Exists(directoryPath))
        {
            Console.WriteLine("Thư mục tồn tại.");
        }
        else
        {
            Console.WriteLine("Thư mục không tồn tại.");
        }
    }
}
```
Kết quả mẫu:
```
Thư mục tồn tại.
```
Hoặc, nếu thư mục không được tìm thấy:
```
Thư mục không tồn tại.
```

## Tìm hiểu sâu hơn
Namespace `System.IO` đã tồn tại từ những ngày đầu của .NET, cung cấp các công cụ cho việc thao tác tệp và thư mục. Khi kiểm tra sự tồn tại của một thư mục, bên trong, nó sử dụng API hệ thống để truy vấn hệ thống tệp - một hoạt động thông thường tiết kiệm về nguồn lực hệ thống.

Còn có class `DirectoryInfo`, cung cấp một cách hướng đối tượng để tương tác với các thư mục. Nó có thể chậm hơn chỉ để kiểm tra sự tồn tại vì nó tạo ra một đối tượng với nhiều dữ liệu hơn chỉ là trạng thái tồn tại, nhưng nó hữu ích cho các thao tác phức tạp hơn.

```C#
DirectoryInfo dirInfo = new DirectoryInfo(directoryPath);
if (dirInfo.Exists)
{
    // Làm gì đó với thư mục.
}
```

Trước `System.IO`, các nhà phát triển có thể đã sử dụng API cụ thể của nền tảng hoặc chạy các tiện ích dòng lệnh để kiểm tra nếu một thư mục tồn tại, cả hai đều rắc rối và đầy rủi ro. `System.IO` đã trừu tượng hóa điều này một cách đẹp đẽ.

Quan trọng là phải lưu ý rằng việc kiểm tra sự tồn tại có thể bị ảnh hưởng bởi điều kiện đua. Chỉ vì một thư mục tồn tại khi bạn kiểm tra không đảm bảo nó sẽ tồn tại một lúc sau khi bạn cố gắng sử dụng nó, do có thể có sự thay đổi bởi các quy trình hoặc người dùng khác.

## Xem thêm
- [Tài liệu MSDN về System.IO.Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Tài liệu MSDN về System.IO.DirectoryInfo](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- [Thảo luận trên StackOverflow về kiểm tra sự tồn tại của thư mục](https://stackoverflow.com/questions/1410127/c-sharp-test-if-user-has-write-access-to-a-folder)

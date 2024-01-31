---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:56:20.947915-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Kiểm tra sự tồn tại của một thư mục trong C# giúp bạn xác định xem một thư mục cụ thể có sẵn trên hệ thống tệp hay không. Các lập trình viên thực hiện điều này để tránh các lỗi như cố gắng đọc từ hoặc ghi vào một thư mục không tồn tại, điều này sẽ khiến chương trình của họ bị lỗi hoặc hoạt động không dự đoán được.

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

---
title:                "Tạo một tập tin tạm thời"
aliases:
- /vi/c-sharp/creating-a-temporary-file/
date:                  2024-01-28T21:58:50.077397-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tạo một tệp tạm thời có nghĩa là bạn đang tạo một tệp cho một mục đích ngắn hạn, như lưu trữ dữ liệu chỉ cần trong quá trình thực thi của một chương trình. Các lập trình viên làm điều này vì nhiều lý do, như tránh hạn chế bộ nhớ hoặc giữ một trạng thái tạm thời trong quá trình thực hiện các thao tác phức tạp.

## Cách làm:
Dưới đây là cách nhanh chóng để tạo và viết vào một tệp tạm thời trong C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Tạo một tệp tạm thời
        string tempFilePath = Path.GetTempFileName();

        // Viết gì đó vào tệp tạm thời
        File.WriteAllText(tempFilePath, "Xin chào, Thế giới Tạm thời!");

        // Đọc lại từ tệp tạm thời và in ra
        string fileContents = File.ReadAllText(tempFilePath);
        Console.WriteLine(fileContents);

        // Dọn dẹp tệp tạm thời
        File.Delete(tempFilePath);
    }
}
```

Kết quả mẫu:
```
Xin chào, Thế giới Tạm thời!
```

## Sâu hơn
Tệp tạm thời đã xuất hiện từ những ngày đầu của lập trình máy tính, khi việc giảm thiểu sử dụng bộ nhớ là cực kỳ quan trọng. Chúng cung cấp một môi trường cô lập cho các chương trình để làm việc với dữ liệu mà không cần lưu trữ lâu dài.

Ngoài `Path.GetTempFileName()`, bạn còn có những lựa chọn khác như `Path.GetRandomFileName()`, không tạo tệp nhưng cho bạn một tên để sử dụng cho một tệp tạm thời. Ngoài ra, lớp `System.IO.TempFileCollection` có thể quản lý nhiều tệp tạm thời, tiện lợi khi bạn cần nhiều hơn một tệp.

Bên dưới lớp vỏ, hầu hết các phương pháp C# tạo tệp tạm thời sử dụng các API cung cấp bởi hệ điều hành cơ bản. Trong Windows, `GetTempFileName()` được ánh xạ vào một hàm API Win32 tương tự giúp đảm bảo tính duy nhất của tên tệp và bảo vệ nó khỏi va chạm.

Hãy nhớ luôn xóa tệp tạm thời. Mặc dù chúng ở trong thư mục tạm, nhưng chúng có thể chất đống nếu bị bỏ qua, trở thành một cơn ác mộng lưu trữ số.

## Xem thêm
Để đọc thêm và hiểu rõ hơn, những liên kết sau đây sẽ giúp bạn bao quát hầu hết mọi thứ bạn cần:

- Tài liệu chính thức của Microsoft về tệp tạm thời trong .NET:
  [Tệp tạm thời trong .NET](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-temporary-file)

- Các phương pháp hay nhất khi làm việc với tệp và luồng trong C#:
  [I/O Tệp và Luồng](https://docs.microsoft.com/en-us/dotnet/standard/io)

- Nếu bạn muốn khám phá các vấn đề về bảo mật I/O tệp:
  [I/O Tệp và Bảo mật](https://docs.microsoft.com/en-us/dotnet/standard/security/secure-file-i-o)

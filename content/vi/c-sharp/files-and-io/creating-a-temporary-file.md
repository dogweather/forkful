---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:50.077397-07:00
description: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129\
  a l\xE0 b\u1EA1n \u0111ang t\u1EA1o m\u1ED9t t\u1EC7p cho m\u1ED9t m\u1EE5c \u0111\
  \xEDch ng\u1EAFn h\u1EA1n, nh\u01B0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u ch\u1EC9\
  \ c\u1EA7n trong qu\xE1 tr\xECnh th\u1EF1c thi c\u1EE7a m\u1ED9t ch\u01B0\u01A1\
  ng\u2026"
lastmod: '2024-03-13T22:44:36.682893-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129\
  a l\xE0 b\u1EA1n \u0111ang t\u1EA1o m\u1ED9t t\u1EC7p cho m\u1ED9t m\u1EE5c \u0111\
  \xEDch ng\u1EAFn h\u1EA1n, nh\u01B0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u ch\u1EC9\
  \ c\u1EA7n trong qu\xE1 tr\xECnh th\u1EF1c thi c\u1EE7a m\u1ED9t ch\u01B0\u01A1\
  ng tr\xECnh."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

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

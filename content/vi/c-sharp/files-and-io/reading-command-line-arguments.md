---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:00.818618-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \"\
  nu\u1ED1t ch\u1EEDng\" nh\u1EEFng \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh."
lastmod: '2024-03-13T22:44:36.677817-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \"nu\u1ED1t ch\u1EEDng\" nh\u1EEF\
  ng \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Dưới đây là cách "nuốt chửng" những đối số dòng lệnh:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Bạn đã nhập các đối số sau:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Nếu bạn chạy chương trình như thế này: `yourapp.exe arg1 arg2 arg3`, hãy mong đợi đầu ra:

```
Bạn đã nhập các đối số sau:
arg1
arg2
arg3
```

## Sâu hơn nữa
Truyền thống của các đối số dòng lệnh bắt nguồn từ bình minh của ngành máy tính, cho phép phần mềm sơ khai linh hoạt. Trong C#, `args` là một mảng chuỗi trong `Main()` chứa các đối số được truyền. Có phương án thay thế không? Chắc chắn rồi, có những thư viện như `CommandLineParser` nâng cấp khả năng, nhưng cho nhiều tác vụ, `args` là người bạn nhanh gọn và dễ dãi.

Bên dưới lớp vỏ, một ứng dụng C# bắt đầu với `Main()`. Khi bạn gọi ứng dụng của mình từ một dòng lệnh hoặc script, hệ điều hành đặt các đối số vào một mảng và truyền nó vào `Main()`. Dễ như ăn bánh.

Bạn có một ứng dụng phức tạp? Có thể bạn cần phân tích các cờ, tuỳ chọn và giá trị? Đó là nơi mà các thư viện tỏa sáng với nhiều kiểm soát và ít mã mẫu cố định hơn so với việc phân tích `args` thô. Nhưng đối với đầu vào đơn giản? `args` suốt đường.

## Xem thêm
- [Microsoft Docs về Main() và các đối số dòng lệnh](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [Thư viện CommandLineParser trên GitHub](https://github.com/commandlineparser/commandline)
- [Cuộc thảo luận trên Stack Overflow về cách phân tích các đối số dòng lệnh trong C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)

---
aliases:
- /vi/c-sharp/reading-a-text-file/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:11.688018-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB m\u1ED9t t\u1EC7p ch\u1EE9a v\u0103n b\u1EA3n. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA3i c\u1EA5u h\xECnh,\
  \ \u0111\u1ECDc d\u1EEF li\u1EC7u ho\u1EB7c n\u1EA1p c\xE1c t\xE0i nguy\xEAn qu\xE1\
  \ c\u1ED3ng k\u1EC1nh\u2026"
lastmod: 2024-02-18 23:08:50.717698
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB m\u1ED9t t\u1EC7p ch\u1EE9a v\u0103n b\u1EA3n. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA3i c\u1EA5u h\xECnh,\
  \ \u0111\u1ECDc d\u1EEF li\u1EC7u ho\u1EB7c n\u1EA1p c\xE1c t\xE0i nguy\xEAn qu\xE1\
  \ c\u1ED3ng k\u1EC1nh\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Đọc một tệp văn bản là lấy dữ liệu từ một tệp chứa văn bản. Lập trình viên làm điều này để tải cấu hình, đọc dữ liệu hoặc nạp các tài nguyên quá cồng kềnh hoặc không phù hợp để mã hóa cứng.

## Cách thực hiện:
Hãy đi thẳng vào vấn đề. Dưới đây là cách bạn đọc từ một tệp trong C# sử dụng `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        
        // Đọc toàn bộ văn bản
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // Đọc các dòng vào một mảng
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // Đọc với StreamReader
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

Kết quả mẫu:

```
Xin chào, đây là một tệp văn bản.
Nó có nhiều dòng.
Mỗi dòng sẽ được đọc riêng biệt.
```

## Tìm hiểu sâu hơn
Đọc một tệp văn bản dường như đơn giản, đúng không? Nhưng có một chút lịch sử và một số điều tinh tế đáng biết.

Ngày xưa, tệp văn bản thường là cách chính để lưu trữ dữ liệu trước khi cơ sở dữ liệu được sử dụng phổ biến. Lập trình viên phải quản lý truy cập tệp, định dạng dữ liệu một cách chính xác và xử lý lỗi. C# đã phát triển rất nhiều kể từ đó. Bây giờ, `System.IO` là không gian tên ưu tiên của bạn cho các thao tác tệp.

Bạn có các lựa chọn:

- `File.ReadAllText` đọc toàn bộ một lần—tuyệt vời cho các tệp nhỏ.
- `File.ReadAllLines` cho bạn mỗi dòng như một phần tử của mảng—tiện lợi cho việc xử lý dòng.
- `StreamReader` đọc từng dòng một, hiệu quả hơn về bộ nhớ cho các tệp lớn.

Mỗi phương pháp sẽ khóa tệp khi nó đang được sử dụng. Điều này quan trọng nếu các quá trình khác có thể đang cố gắng truy cập tệp.

Nhớ là luôn xử lý các ngoại lệ như `FileNotFoundException` hoặc `IOException` khi làm việc với tệp. Bạn không muốn ứng dụng của mình bất ngờ gặp sự cố.

## Xem thêm
Có thêm câu hỏi hoặc muốn mở rộng kiến thức của bạn? Hãy xem các liên kết sau:

- [Tài liệu MSDN về Lớp File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Tài liệu MSDN về Lớp StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Hướng dẫn về xử lý ngoại lệ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)

---
title:                "Làm việc với CSV"
aliases: - /vi/c-sharp/working-with-csv.md
date:                  2024-01-28T22:10:41.641805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với CSV (Comma-Separated Values) có nghĩa là đọc và viết dữ liệu trong một định dạng văn bản đơn giản — một định dạng phổ biến và thân thiện với bảng tính. Lập trình viên sử dụng CSV vì sự đơn giản và khả năng tương thích khi trao đổi dữ liệu dạng bảng giữa các hệ thống.

## Làm thế nào

### Đọc File CSV
```C#
using System;
using System.IO;

class ReadCSVExample
{
    static void Main()
    {
        string path = "data.csv";
        using (var reader = new StreamReader(path))
        {
            while (!reader.EndOfStream)
            {
                var line = reader.ReadLine();
                var values = line.Split(',');
                // Bây giờ làm gì đó với giá trị, ví dụ, in chúng ra
                Console.WriteLine(String.Join(" | ", values));
            }
        }
    }
}
```
**Kết quả mẫu:**
```
John | Doe | johndoe@example.com
Jane | Smith | janesmith@example.com
```

### Viết File CSV
```C#
using System;
using System.IO;

class WriteCSVExample
{
    static void Main()
    {
        string path = "output.csv";
        var records = new[]
        {
            new[] {"Name", "Age", "Email"},
            new[] {"Alice", "23", "alice@example.com"},
            new[] {"Bob", "30", "bob@example.com"}
        };

        using (var writer = new StreamWriter(path))
        {
            foreach (var record in records)
            {
                var line = String.Join(",", record);
                writer.WriteLine(line);
            }
        }
        Console.WriteLine($"Dữ liệu đã được viết vào {path}");
    }
}
```
**Kết quả mẫu:**
```
Dữ liệu đã được viết vào output.csv
```

## Tìm hiểu sâu hơn

CSV đã xuất hiện từ những ngày đầu của ngành công nghiệp máy tính, tạo cầu nối giữa các hệ thống đa dạng. Nó không hoàn hảo — thiếu chuẩn mã hóa ký tự và không hỗ trợ tốt các trường nhiều dòng mà không có một trình phân tích cú pháp mạnh mẽ. Đó là nơi các định dạng như JSON và XML bước vào, cung cấp nhiều phức tạp hơn nhưng cấu trúc tốt hơn cho dữ liệu phân cấp.

Ở dưới cùng, bạn thường xử lý chuỗi, hoặc sử dụng các phương thức `string` có sẵn hoặc thư viện như `CsvHelper` có thể thêm sức mạnh vào việc xử lý CSV của bạn, cung cấp nhiều tính năng hơn và xử lý tình huống ngoại lệ một cách nhẹ nhàng. Nhớ lại, không có việc xử lý CSV bản địa trong .NET, vì vậy bạn tự mình xử lý chuỗi hoặc bạn có thể chọn một thư viện bên thứ ba.

## Xem thêm

Để tìm hiểu sâu hơn về việc thao tác CSV trong C#:
- [Thư viện CsvHelper](https://joshclose.github.io/CsvHelper/)
- [Tài liệu của Microsoft về `StreamReader`](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)

Tìm hiểu nhiều hơn về các phương án thay thế cho CSV:
- [Hiểu về JSON](https://www.json.org/json-en.html)
- [XML trong một Lõi Đậu](https://www.w3schools.com/xml/xml_whatis.asp)

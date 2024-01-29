---
title:                "Chuyển đổi một ngày thành chuỗi"
date:                  2024-01-28T21:57:23.207611-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi ngày tháng thành chuỗi trong C# là quá trình thay đổi định dạng từ đối tượng DateTime sang biểu diễn văn bản. Lập trình viên thực hiện điều này để hiển thị ngày tháng theo định dạng thân thiện với người dùng hoặc để tuần tự hóa dữ liệu cho việc lưu trữ và truyền tải.

## Làm thế nào:

Trong C#, bạn có đối tượng `DateTime` và nhiều cách để chuyển nó thành chuỗi. Dưới đây là một số ví dụ:

```csharp
DateTime now = DateTime.Now;
string defaultString = now.ToString(); // Định dạng mặc định
string specificFormat = now.ToString("yyyy-MM-dd"); // Định dạng tùy chỉnh, ở đây là ISO 8601
string withCulture = now.ToString("d", new CultureInfo("en-US")); // Ngày ngắn theo văn hóa Mỹ

Console.WriteLine(defaultString); // Kết quả phụ thuộc vào cài đặt văn hóa của hệ thống
Console.WriteLine(specificFormat); // Kết quả: "2023-04-01"
Console.WriteLine(withCulture); // Kết quả: "4/1/2023"
```

## Sâu hơn

Trước kia, việc thao tác với ngày tháng và chuỗi khó khăn hơn. Ngày nay, `DateTime` của C# cung cấp `.ToString()` với các overloads cho văn hóa và định dạng. Giao diện `IFormatProvider`, như `CultureInfo`, kiểm soát định dạng đặc biệt cho văn hóa.

Có phương án thay thế? Chắc chắn! `String.Format` và nội suy (`$"{now:yyyy-MM-dd}"`) là các lựa chọn cho việc chèn ngày tháng vào chuỗi với ngữ cảnh. `DateTimeOffset` hữu ích cho các đặc điểm về múi giờ.

Về mặt triển khai, hãy nhớ rằng `DateTime` là một struct, do đó là kiểu giá trị. Việc chuyển đổi nó không thay đổi bản gốc: tính bất biến chiến thắng. Chọn định dạng chuỗi của bạn một cách khôn ngoan dựa trên đối tượng người dùng (người dùng cuối) và hệ thống bạn đang tương tác (cơ sở dữ liệu, API).

## Xem thêm

- [Phương thức DateTime.ToString](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Chuỗi định dạng ngày và giờ tùy chỉnh](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Lớp CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)

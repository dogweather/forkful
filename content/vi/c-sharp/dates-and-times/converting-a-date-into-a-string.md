---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:23.207611-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C#, b\u1EA1n c\xF3 \u0111\u1ED1i t\u01B0\
  \u1EE3ng `DateTime` v\xE0 nhi\u1EC1u c\xE1ch \u0111\u1EC3 chuy\u1EC3n n\xF3 th\xE0\
  nh chu\u1ED7i. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t s\u1ED1 v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:36.672591-06:00'
model: gpt-4-0125-preview
summary: "Trong C#, b\u1EA1n c\xF3 \u0111\u1ED1i t\u01B0\u1EE3ng `DateTime` v\xE0\
  \ nhi\u1EC1u c\xE1ch \u0111\u1EC3 chuy\u1EC3n n\xF3 th\xE0nh chu\u1ED7i."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

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

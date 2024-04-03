---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:14.987291-07:00
description: "C\xE1ch Th\u1EF1c Hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.670079-06:00'
model: gpt-4-0125-preview
summary: .
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Cách Thực Hiện:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023-03-15";
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate); // Kết quả: 3/15/2023 12:00:00 SA

        // Với định dạng cụ thể
        dateString = "15 March, 2023";
        string format = "d MMMM, yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;
        parsedDate = DateTime.ParseExact(dateString, format, provider);
        Console.WriteLine(parsedDate); // Kết quả: 3/15/2023 12:00:00 SA
    }
}
```

## Tìm Hiểu Sâu
Trước khi có `DateTime`, các lập trình viên dựa vào mã tự tạo để xử lý các ngày, điều này dễ dẫn đến lỗi và không hiệu quả. Cấu trúc `DateTime` trong .NET đã cách mạng hóa điều này, cung cấp các phương pháp phân tích cú pháp mạnh mẽ - `Parse` và `ParseExact`.

`Parse` cố gắng hiểu một chuỗi ngày dựa trên các định dạng cụ thể của văn hóa hoặc toàn cầu. Tuyệt vời khi bạn mong đợi các định dạng ngày tiêu chuẩn. Tuy nhiên, nếu bạn có các định dạng ngày cụ thể hoặc không thông thường, `ParseExact` (cùng với `TryParse` và `TryParseExact` để xử lý lỗi) sẽ là sự lựa chọn hoàn hảo cho bạn. Tại đây, bạn chỉ định định dạng chính xác với một mẫu tự tạo.

Thực thi sử dụng lớp `CultureInfo` để tôn trọng các định dạng ngày của các nền văn hóa khác nhau. Khi sử dụng `ParseExact`, bạn tránh những hiểu nhầm văn hóa - mẫu bạn định nghĩa là điều quyết định. Nhớ rằng, ngày trên máy tính bắt đầu từ ngày 1 tháng 1 năm 0001, vì vậy hãy đảm bảo chuỗi của bạn đại diện cho một ngày hợp lệ trong phạm vi lịch của .NET.

## Xem Thêm
- [Tài Liệu Phương Thức DateTime.Parse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse)
- [Tài Liệu Phương Thức DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- [Chuỗi Định Dạng Ngày và Giờ Tùy Chỉnh](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Lớp CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)

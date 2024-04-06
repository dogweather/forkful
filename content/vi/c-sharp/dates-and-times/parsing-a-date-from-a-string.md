---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:14.987291-07:00
description: "C\xE1ch Th\u1EF1c Hi\u1EC7n: Tr\u01B0\u1EDBc khi c\xF3 `DateTime`, c\xE1\
  c l\u1EADp tr\xECnh vi\xEAn d\u1EF1a v\xE0o m\xE3 t\u1EF1 t\u1EA1o \u0111\u1EC3\
  \ x\u1EED l\xFD c\xE1c ng\xE0y, \u0111i\u1EC1u n\xE0y d\u1EC5 d\u1EABn \u0111\u1EBF\
  n l\u1ED7i v\xE0 kh\xF4ng hi\u1EC7u qu\u1EA3. C\u1EA5u tr\xFAc\u2026"
lastmod: '2024-04-05T21:53:38.059596-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc khi c\xF3 `DateTime`, c\xE1c l\u1EADp tr\xECnh vi\xEAn d\u1EF1\
  a v\xE0o m\xE3 t\u1EF1 t\u1EA1o \u0111\u1EC3 x\u1EED l\xFD c\xE1c ng\xE0y, \u0111\
  i\u1EC1u n\xE0y d\u1EC5 d\u1EABn \u0111\u1EBFn l\u1ED7i v\xE0 kh\xF4ng hi\u1EC7\
  u qu\u1EA3."
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

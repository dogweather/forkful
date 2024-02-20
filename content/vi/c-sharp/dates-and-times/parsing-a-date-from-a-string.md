---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:14.987291-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p m\u1ED9t ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7\
  i ngh\u0129a l\xE0 chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n m\xF4 t\u1EA3 m\u1ED9\
  t ng\xE0y th\xE0nh m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng `DateTime`. \u0110i\u1EC1\
  u n\xE0y r\u1EA5t quan tr\u1ECDng \u0111\u1EC3 l\u01B0u v\xE0 di\u1EC5n\u2026"
lastmod: 2024-02-19 22:04:55.845260
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p m\u1ED9t ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7\
  i ngh\u0129a l\xE0 chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n m\xF4 t\u1EA3 m\u1ED9\
  t ng\xE0y th\xE0nh m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng `DateTime`. \u0110i\u1EC1\
  u n\xE0y r\u1EA5t quan tr\u1ECDng \u0111\u1EC3 l\u01B0u v\xE0 di\u1EC5n\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Phân tích cú pháp một ngày từ một chuỗi nghĩa là chuyển đổi văn bản mô tả một ngày thành một đối tượng `DateTime`. Điều này rất quan trọng để lưu và diễn giải các ngày từ nhiều định dạng khác nhau như là những ngày thực sự trong mã của bạn.

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

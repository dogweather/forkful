---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases:
- /vi/c-sharp/parsing-a-date-from-a-string/
date:                  2024-01-28T22:04:14.987291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:50.838827-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1\
  ng lai."
lastmod: '2024-03-13T22:44:36.675174-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cách thực hiện:
Tính toán ngày trong tương lai:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan oneWeek = TimeSpan.FromDays(7);
        
        DateTime nextWeek = currentDate + oneWeek;
        Console.WriteLine($"Một tuần từ bây giờ: {nextWeek}");
    }
}
```

Kết quả:

```
Một tuần từ bây giờ: <ngày sau một tuần từ ngày hiện tại>
```

Tính toán ngày trong quá khứ:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan tenDaysAgo = TimeSpan.FromDays(-10);
        
        DateTime pastDate = currentDate + tenDaysAgo;
        Console.WriteLine($"Mười ngày trước là: {pastDate}");
    }
}
```

Kết quả:

```
Mười ngày trước là: <ngày mười ngày trước từ ngày hiện tại>
```

## Sâu hơn nữa
Trong C#, `DateTime` và `TimeSpan` là nền tảng cho các thao tác về ngày và thời gian. `DateTime` đại diện cho một khoảnh khắc trong thời gian, thường được biểu thị dưới dạng ngày và giờ của ngày. `TimeSpan` đại diện cho một khoảng thời gian.

Trong lịch sử, các phép toán về ngày và giờ thường dễ mắc lỗi do việc xử lý thủ công các ngày, tháng, và năm nhuận. `DateTime` tóm gọn những phức tạp này, cho phép framework xử lý các phần khó khăn.

Các lựa chọn thay thế cho `DateTime` và `TimeSpan` trong .NET bao gồm `DateTimeOffset`, bao gồm độ lệch múi giờ, làm cho nó tốt hơn cho các ứng dụng hoạt động qua các múi giờ. Một lựa chọn khác là Noda Time, một thư viện được thiết kế bởi Jon Skeet dành cho việc xử lý ngày và giờ phức tạp hơn, như sử dụng các lịch khác nhau.

Về mặt thực hiện, khi bạn thêm một `TimeSpan` vào một `DateTime`, bên dưới nó đang thao tác với các tick, đơn vị cơ bản của thời gian trong .NET (`1 tick = 100 nanô giây`). Đối với các ngày trong quá khứ, một `TimeSpan` âm sẽ giải quyết vấn đề.

## Xem thêm
- Tài liệu API .NET cho [`DateTime`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- Giới thiệu về [`TimeSpan`](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- Các phương pháp tốt nhất của Microsoft cho [`DateTime` và `DateTimeOffset`](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- Tài liệu Noda Time: [https://nodatime.org](https://nodatime.org)

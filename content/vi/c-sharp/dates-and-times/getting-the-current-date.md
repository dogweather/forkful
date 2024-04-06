---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:30.803332-07:00
description: "L\xE0m th\u1EBF n\xE0o: L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i? Ch\u1EC9\
  \ c\u1EA7n g\u1ECDi `DateTime.Now`. \u0110o\u1EA1n m\xE3 n\xE0y hi\u1EC3n th\u1ECB\
  \ c\xE1ch l\xE0m."
lastmod: '2024-04-05T21:53:38.061008-06:00'
model: gpt-4-0125-preview
summary: ''
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Làm thế nào:
Lấy ngày hiện tại? Chỉ cần gọi `DateTime.Now`. Đoạn mã này hiển thị cách làm:

```C#
using System;

class GetCurrentDate
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```

Nếu bạn chạy nó, dự đoán kết quả như thế này:

```
3/25/2023 11:34:52 AM
```

Thú vị, phải không?

## Tìm hiểu sâu hơn
Trước `DateTime`, các lập trình viên phải tự tính toán thời gian trong đầu. Giờ đây, .NET đã làm cho nó trở nên dễ dàng hơn. `DateTime.Now` bắt cả ngày và giờ, nhưng cho chỉ ngày, thì có `DateTime.Today`.

Đây là điều thú vị - nó tôn trọng múi giờ. `DateTime.UtcNow` cung cấp thời gian Coordinated Universal Time (UTC), tránh vấn đề thời gian địa phương.

Về mặt lịch sử, việc giữ thời gian là một hỗn loạn - nghĩ về đồng hồ mặt trời, đồng hồ nước, tên gì đi nữa. Máy tính đã làm cho nó đơn giản hóa, nhưng múi giờ và các quy tắc tiết kiệm ánh sáng ban ngày vẫn làm phức tạp các vấn đề. May mắn thay, C# được trang bị `TimeZoneInfo` nếu bạn cần xoay xở với các múi giờ.

Ngoài `DateTime`, chúng ta còn có `DateTimeOffset`. Nó kết hợp ngày giờ với độ lệch so với UTC, hữu ích nếu bạn quan tâm đến độ chính xác của múi giờ.

Về mặt thực hiện, `DateTime` trong C# chính xác đến 100-nanosecond ticks kể từ nửa đêm, ngày 1 tháng 1, năm 0001 Công Nguyên. Nhưng đừng lên kế hoạch cho nanosecond của bạn dựa vào nó - độ chính xác và độ chính xác của đồng hồ hệ thống có thể thay đổi rất nhiều.

## Xem thêm
- [Cấu trúc DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
- [Thuộc tính DateTime.UtcNow](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.utcnow?view=net-7.0)
- [Cấu trúc DateTimeOffset](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-7.0)
- [Lớp TimeZoneInfo](https://docs.microsoft.com/en-us/dotnet/api/system.timezoneinfo?view=net-7.0)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:59.316679-07:00
description: "Logging l\xE0 qu\xE1 tr\xECnh ghi ch\xE9p c\xE1c s\u1EF1 ki\u1EC7n v\xE0\
  \ d\u1EEF li\u1EC7u \u0111\u1EA7u ra c\u1EE7a \u1EE9ng d\u1EE5ng trong qu\xE1 tr\xEC\
  nh ch\u1EA1y. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n logging \u0111\u1EC3\
  \ ch\u1EA9n \u0111o\xE1n l\u1ED7i, theo d\xF5i\u2026"
lastmod: '2024-03-13T22:44:36.666065-06:00'
model: gpt-4-0125-preview
summary: "Logging l\xE0 qu\xE1 tr\xECnh ghi ch\xE9p c\xE1c s\u1EF1 ki\u1EC7n v\xE0\
  \ d\u1EEF li\u1EC7u \u0111\u1EA7u ra c\u1EE7a \u1EE9ng d\u1EE5ng trong qu\xE1 tr\xEC\
  nh ch\u1EA1y."
title: Ghi log
weight: 17
---

## Làm thế nào:
Trong C#, bạn có thể sử dụng không gian tên `System.Diagnostics` có sẵn hoặc các thư viện bên thứ ba như NLog hoặc log4net. Dưới đây là một ví dụ nhanh sử dụng giao diện `ILogger` có sẵn trong .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Đây là một thông điệp thông tin.");
        logger.LogWarning("Đây là một thông điệp cảnh báo.");
        logger.LogError("Đây là một thông điệp lỗi.");
    }
}
```

Kết quả mẫu:
```
info: Program[0]
      Đây là một thông điệp thông tin.
warn: Program[0]
      Đây là một thông điệp cảnh báo.
fail: Program[0]
      Đây là một thông điệp lỗi.
```

## Sâu hơn
Lịch sử của logging trong phát triển phần mềm gần như tuổi đời của lập trình; nó đã phát triển từ các câu lệnh in đơn giản đến các hệ thống cấu hình phức tạp. Ban đầu, logging được thực hiện bằng cách viết vào tệp hoặc console, nhưng điều này đã phát triển bao gồm các cấu trúc phức tạp hơn như hệ thống tổng hợp log và các nền tảng truy vấn phân tán (như ELK stack hoặc Jaeger).

Các lựa chọn thay thế cho tính năng logging có sẵn trong .NET bao gồm các thư viện bên thứ ba:
- **NLog**: linh hoạt và dễ dàng thiết lập, với nhiều tính năng cho việc định tuyến, định dạng, và lọc log.
- **log4net**: lấy cảm hứng từ thư viện log4j của Java, nó có thể cấu hình mạnh mẽ từ XML và hỗ trợ một loạt các kho lưu trữ log.

Khi nói đến chi tiết thực hiện, lựa chọn giao diện trừu tượng logging của bạn (như Microsoft.Extensions.Logging) và nhà cung cấp logging cơ bản có thể ảnh hưởng đáng kể đến hiệu suất và độ tin cậy của ứng dụng của bạn. Điều quan trọng là cần cấu hình mức độ logging một cách phù hợp và đảm bảo việc viết log không trở thành điểm nghẽn.

Ngoài ra, structured logging - nơi bạn ghi log không chỉ là chuỗi mà còn là cặp khóa-giá trị hoặc đối tượng - cho phép log chính xác và có thể thực hiện hơn, dễ dàng hơn trong việc truy vấn và phân tích.

## Xem thêm
- [Tài liệu Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [Tài liệu NLog](https://nlog-project.org/documentation/)
- [Tài liệu log4net](https://logging.apache.org/log4net/)
- [Tài liệu Serilog](https://serilog.net/) (ví dụ về structured logging)

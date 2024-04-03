---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:59.316679-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C#, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5\
  ng kh\xF4ng gian t\xEAn `System.Diagnostics` c\xF3 s\u1EB5n ho\u1EB7c c\xE1c th\u01B0\
  \ vi\u1EC7n b\xEAn th\u1EE9 ba nh\u01B0 NLog ho\u1EB7c log4net. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:36.666065-06:00'
model: gpt-4-0125-preview
summary: "Trong C#, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng kh\xF4ng gian t\xEA\
  n `System.Diagnostics` c\xF3 s\u1EB5n ho\u1EB7c c\xE1c th\u01B0 vi\u1EC7n b\xEA\
  n th\u1EE9 ba nh\u01B0 NLog ho\u1EB7c log4net."
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:31.869478-07:00
description: "X\u1EED l\xFD l\u1ED7i trong C# l\xE0 v\u1EC1 vi\u1EC7c qu\u1EA3n l\xFD\
  \ nh\u1EEFng \u0111i\u1EC1u kh\xF4ng mong \u0111\u1EE3i - gi\u1ED1ng nh\u01B0 v\u1EA5\
  p ph\u1EA3i d\xE2y gi\xE0y c\u1EE7a m\xECnh. Ch\u01B0\u01A1ng tr\xECnh c\xF3 th\u1EC3\
  \ v\u1EA5p ph\u1EA3i d\u1EEF li\u1EC7u x\u1EA5u ho\u1EB7c k\u1EBFt\u2026"
lastmod: '2024-02-25T18:49:35.010393-07:00'
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i trong C# l\xE0 v\u1EC1 vi\u1EC7c qu\u1EA3n l\xFD\
  \ nh\u1EEFng \u0111i\u1EC1u kh\xF4ng mong \u0111\u1EE3i - gi\u1ED1ng nh\u01B0 v\u1EA5\
  p ph\u1EA3i d\xE2y gi\xE0y c\u1EE7a m\xECnh. Ch\u01B0\u01A1ng tr\xECnh c\xF3 th\u1EC3\
  \ v\u1EA5p ph\u1EA3i d\u1EEF li\u1EC7u x\u1EA5u ho\u1EB7c k\u1EBFt\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Xử lý lỗi trong C# là về việc quản lý những điều không mong đợi - giống như vấp phải dây giày của mình. Chương trình có thể vấp phải dữ liệu xấu hoặc kết nối kém. Chúng ta xử lý lỗi để ngăn chương trình của mình "ngã mặt", cho phép nó phục hồi một cách duyên dáng.

## Làm Thế Nào:

Hãy bắt đầu với khối try-catch. Nó giống như việc đặt một lưới an toàn dưới người đi trên dây. Nếu họ trượt chân, họ không rơi xuống - họ được bắt lấy.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // Ôi, chỉ số vượt quá giới hạn!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Bắt được lỗi: " + e.Message);
        }
    }
}
```

Kết quả mẫu khi mọi thứ đi chệch hướng:
```
Bắt được lỗi: Chỉ số vượt quá giới hạn của mảng.
```

Bây giờ chúng ta thêm một khối finally - đó là những gì xảy ra dù thế nào, giống như việc phải đóng thuế.

```C#
try {
    // Mã có thể gây rắc rối ở đây
} catch (SomeSpecificException e) {
    // Xử lý lỗi cụ thể ở đây
} finally {
    // Mã này chạy dù điều gì xảy ra phía trên
    Console.WriteLine("Luôn luôn chạy.");
}
```

## Đào Sâu

Xử lý lỗi đã có trong C# kể từ khi nó ra đời. Theo thời gian, nó đã phát triển. Ngày xưa, lập trình viên dựa vào mã trả về hoặc cờ toàn cục để báo hiệu vấn đề - cồng kềnh và dễ phạm lỗi.

C# sử dụng ngoại lệ, một cách tiếp cận hiện đại hơn. Một ngoại lệ được ném ra khi điều không mong đợi xảy ra, giống như việc ném một lá cờ vào sân trong bóng đá. Xử lý ngoại lệ cấu trúc với các khối try, catch, và finally làm cho việc quản lý những khoảnh khắc này rõ ràng và sạch sẽ hơn kiểm tra lỗi kiểu cũ.

Có phương án thay thế không? Chắc chắn rồi. Có `UnhandledExceptionEventHandler` cho những ngoại lệ lọt qua. Hoặc trong mã bất đồng bộ, xử lý lỗi được xoay chuyển một chút với các đối tượng `Task` mang theo định mệnh ngoại lệ của riêng chúng.

Chi tiết triển khai - tương tự như in nhỏ - quan trọng. Ngoại lệ có thể tốn kém, làm giảm hiệu suất nếu được ném một cách tùy tiện. Vì vậy, chúng ta sử dụng chúng cho những trường hợp ngoại lệ, không phải để kiểm soát logic hàng ngày.

## Xem Thêm

- [Tài liệu chính thức về Ngoại lệ trong C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Các phương pháp tốt nhất trong xử lý ngoại lệ C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)

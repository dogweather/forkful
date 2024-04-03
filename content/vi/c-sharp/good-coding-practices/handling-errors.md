---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:31.869478-07:00
description: "L\xE0m Th\u1EBF N\xE0o: H\xE3y b\u1EAFt \u0111\u1EA7u v\u1EDBi kh\u1ED1\
  i try-catch. N\xF3 gi\u1ED1ng nh\u01B0 vi\u1EC7c \u0111\u1EB7t m\u1ED9t l\u01B0\u1EDB\
  i an to\xE0n d\u01B0\u1EDBi ng\u01B0\u1EDDi \u0111i tr\xEAn d\xE2y. N\u1EBFu h\u1ECD\
  \ tr\u01B0\u1EE3t ch\xE2n, h\u1ECD kh\xF4ng r\u01A1i xu\u1ED1ng - h\u1ECD\u2026"
lastmod: '2024-03-13T22:44:36.667355-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y b\u1EAFt \u0111\u1EA7u v\u1EDBi kh\u1ED1i try-catch."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

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

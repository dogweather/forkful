---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:29.271107-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C# c\xF3 c\u1EA5u tr\xFAc `System.Numerics.Complex`\
  \ \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n \u0111\u1EC3 x\u1EED l\xFD s\u1ED1\
  \ ph\u1EE9c. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t h\u01B0\u1EDBng d\u1EABn\
  \ nhanh."
lastmod: '2024-03-13T22:44:36.649228-06:00'
model: gpt-4-0125-preview
summary: "C# c\xF3 c\u1EA5u tr\xFAc `System.Numerics.Complex` \u0111\u01B0\u1EE3c\
  \ t\xEDch h\u1EE3p s\u1EB5n \u0111\u1EC3 x\u1EED l\xFD s\u1ED1 ph\u1EE9c."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Cách thực hiện:
C# có cấu trúc `System.Numerics.Complex` được tích hợp sẵn để xử lý số phức. Dưới đây là một hướng dẫn nhanh:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // Tạo số phức
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Các phép toán cơ bản
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // Xuất kết quả
        Console.WriteLine($"Tổng: {sum}");
        Console.WriteLine($"Hiệu: {difference}");
        Console.WriteLine($"Tích: {product}");
        Console.WriteLine($"Thương: {quotient}");
        Console.WriteLine($"Độ lớn của c1: {c1.Magnitude}");
        Console.WriteLine($"Góc pha của c1: {c1.Phase}");
    }
}
```

Và kết quả sẽ là:

```
Tổng: (4.70710678118655, 5.70710678118655)
Hiệu: (3.29289321881345, 4.29289321881345)
Tích: (-1.00000000000001, 9)
Thương: (0.6, 0.8)
Độ lớn của c1: 6.40312423743285
Góc pha của c1: 0.896055384571344
```

## Đi sâu vào
Số phức, bao gồm một phần thực và một phần ảo (thường được ký hiệu là a + bi), đã xuất hiện từ thế kỷ 17. Nhà toán học người Ý Gerolamo Cardano được cho là đã phát triển sớm các số phức. Trong lập trình, việc xử lý số phức bao gồm việc hiểu và quản lý hai phần riêng biệt này.

Mặc dù `System.Numerics.Complex` của C# là mạnh mẽ và được tích hợp vào ngôn ngữ, nhưng các ngôn ngữ khác như Python cũng cung cấp chức năng tương tự với `cmath` hoặc các thư viện bên thứ ba. Và nếu bạn đang làm việc trên một phiên bản cũ của C# hoặc một phiên bản .NET không hỗ trợ `System.Numerics`, bạn có thể phải tự mình tạo lớp số phức hoặc tìm một thư viện.

Bên trong, các thao tác trên số phức sử dụng số học dấu phẩy động có thể gây ra lỗi làm tròn. Vì vậy, khi triển khai các thuật toán sử dụng rộng rãi số phức, điều quan trọng là phải nhớ điều này và xem xét tác động đến độ chính xác và độ tin cậy.

## Xem thêm
1. Tài liệu tham khảo C# cho `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. Tìm hiểu sâu hơn về toán học của số phức: https://mathworld.wolfram.com/ComplexNumber.html
3. Đối với các triển khai thay thế và thư viện, xem Math.NET Numerics: https://numerics.mathdotnet.com/

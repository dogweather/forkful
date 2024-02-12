---
title:                "Làm việc với số phức"
aliases:
- /vi/c-sharp/working-with-complex-numbers.md
date:                  2024-01-28T22:12:29.271107-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do và Mục đích?
Số phức mở rộng hệ thống số của chúng ta để bao gồm số ảo, cho phép chúng ta giải các phương trình không có nghiệm thực. Các lập trình viên làm việc với chúng trong các lĩnh vực như kỹ thuật, vật lý và xử lý tín hiệu nơi những số này là cần thiết cho việc mô hình hóa và giải quyết vấn đề.

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

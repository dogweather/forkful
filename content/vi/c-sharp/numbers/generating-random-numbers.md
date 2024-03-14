---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:18.961038-07:00
description: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong C# bao\
  \ g\u1ED3m vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB s\u1ED1 kh\xF4ng th\u1EC3\
  \ \u0111o\xE1n tr\u01B0\u1EDBc trong m\u1ED9t ph\u1EA1m vi \u0111\xE3 x\xE1c \u0111\
  \u1ECBnh. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.652015-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong C# bao g\u1ED3\
  m vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB s\u1ED1 kh\xF4ng th\u1EC3 \u0111\
  o\xE1n tr\u01B0\u1EDBc trong m\u1ED9t ph\u1EA1m vi \u0111\xE3 x\xE1c \u0111\u1ECB\
  nh. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng c\xE1c\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo ra các số ngẫu nhiên trong C# bao gồm việc tạo ra các giá trị số không thể đoán trước trong một phạm vi đã xác định. Lập trình viên sử dụng các phương pháp này để thực hiện các tính năng như mã hóa, mô phỏng, và trò chơi, nơi mà sự không thể đoán trước hoặc việc mô phỏng sự ngẫu nhiên trong thế giới thực là cần thiết.

## Làm thế nào:

Cách phổ biến nhất để tạo số ngẫu nhiên trong C# là sử dụng lớp `System.Random`. Dưới đây là một ví dụ đơn giản minh họa cách sử dụng:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Tạo ra một số giữa 1 và 99
        Console.WriteLine($"Số ngẫu nhiên: {randomNumber}");
    }
}
```

Điều này sẽ xuất ra một số ngẫu nhiên, như:

```
Số ngẫu nhiên: 42
```

Để tạo một số thực dấu phẩy động ngẫu nhiên giữa 0.0 và 1.0, bạn có thể sử dụng phương thức `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Số thực ngẫu nhiên: {randomDouble}");
```

Nếu bạn đang làm việc trên một ứng dụng nhạy cảm với bảo mật yêu cầu sự ngẫu nhiên mật mã hóa, sẽ tốt hơn nếu bạn sử dụng lớp `RNGCryptoServiceProvider` tìm trong `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Tạo một số ngẫu nhiên dài 4 byte 
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Số ngẫu nhiên mật mã hóa an toàn: {value}");
    }
}
```

## Đi sâu vào vấn đề

Quá trình tạo số ngẫu nhiên trong C# đã phát triển theo năm tháng. Ban đầu, lớp `System.Random` là lựa chọn hàng đầu để tạo ra các số giả ngẫu nhiên. Nó là giả ngẫu nhiên bởi vì, với một giá trị hạt giống cụ thể, nó sẽ tạo ra cùng một chuỗi số, điều này có thể hữu ích cho việc gỡ lỗi hoặc lặp lại các bài kiểm tra.

Mặc dù đủ tốt cho các nhu cầu cơ bản, `System.Random` không an toàn với nhiều luồng và có thể tạo ra kết quả dễ đoán, điều này không phù hợp cho các ứng dụng phụ thuộc vào bảo mật. Hạn chế này đã dẫn đến sự ra đời của `RNGCryptoServiceProvider` cho sự ngẫu nhiên mật mã hóa, cái mà an toàn hơn nhưng cũng tiêu tốn nhiều tài nguyên hơn.

Một lựa chọn khác trong .NET Core và .NET 5+ là lớp `RandomNumberGenerator` trong `System.Security.Cryptography` để tạo số ngẫu nhiên một cách an toàn, nhằm mục đích là một lựa chọn hiện đại và dễ sử dụng hơn so với `RNGCryptoServiceProvider`.

Mỗi phương pháp tạo số ngẫu nhiên trong C# có vị trí phù hợp tùy thuộc vào yêu cầu của ứng dụng. Đối với hầu hết các ứng dụng, `System.Random` là đủ, nhưng đối với những ứng dụng yêu cầu các số ngẫu nhiên an toàn, không thể đoán trước, các lớp mã hóa cung cấp một lựa chọn mạnh mẽ.

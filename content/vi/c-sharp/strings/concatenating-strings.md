---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:01.261588-07:00
description: "N\u1ED1i chu\u1ED7i l\xE0 qu\xE1 tr\xECnh gh\xE9p c\xE1c chu\u1ED7i\
  \ l\u1EA1i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. Ch\xFAng\
  \ ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y v\xEC th\u01B0\u1EDDng xuy\xEAn ch\xFA\
  ng ta c\u1EA7n k\u1EBFt h\u1EE3p c\xE1c t\u1EEB ho\u1EB7c bi\u1EC3u\u2026"
lastmod: '2024-03-11T00:14:09.921573-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i l\xE0 qu\xE1 tr\xECnh gh\xE9p c\xE1c chu\u1ED7i l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. Ch\xFAng ta th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y v\xEC th\u01B0\u1EDDng xuy\xEAn ch\xFAng ta c\u1EA7\
  n k\u1EBFt h\u1EE3p c\xE1c t\u1EEB ho\u1EB7c bi\u1EC3u\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi là quá trình ghép các chuỗi lại với nhau từ đầu đến cuối. Chúng ta thực hiện điều này vì thường xuyên chúng ta cần kết hợp các từ hoặc biểu tượng để tạo ra cụm từ, thông điệp, hoặc tính toán các giá trị động thành văn bản dễ đọc.

## Cách thực hiện:

Nối chuỗi trong C# có thể được thực hiện theo vài cách:

Sử dụng toán tử `+`:
```C#
string hello = "Hello";
string world = "World";
string concatenated = hello + ", " + world + "!";
Console.WriteLine(concatenated); // Đầu ra: Hello, World!
```

Sử dụng phương thức `String.Concat()`:
```C#
string concatenated = String.Concat("Hello", ", ", "World", "!");
Console.WriteLine(concatenated); // Đầu ra: Hello, World!
```

Sử dụng `StringBuilder` để tăng hiệu quả trong các vòng lặp:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(", ");
sb.Append("World");
sb.Append("!");
Console.WriteLine(sb.ToString()); // Đầu ra: Hello, World!
```

Sử dụng nội suy chuỗi (C# 6.0 trở lên):
```C#
string world = "World";
string concatenated = $"Hello, {world}!";
Console.WriteLine(concatenated); // Đầu ra: Hello, World!
```

## Sâu hơn nữa

Nối chuỗi không phải là điều mới mẻ; nó đã được sử dụng từ những ngày đầu của lập trình. Tuy nhiên, cách chúng ta thực hiện nó trong C# đã phát triển. Ban đầu, toán tử `+` được sử dụng rộng rãi, nhưng không phải lúc nào cũng hiệu quả, đặc biệt là trong các vòng lặp, vì chuỗi trong .NET là bất biến. Mỗi thao tác `+` tạo ra một chuỗi mới, có thể dẫn đến các vấn đề về hiệu suất.

`String.Concat()` là một lời gọi phương thức trực tiếp cũng không thân thiện với vòng lặp nhưng ổn cho một số lượng chuỗi nhỏ và cố định.

`StringBuilder` là lựa chọn hàng đầu cho các tình huống vòng lặp hoặc khi xây dựng chuỗi một cách tăng dần. Đằng sau cánh gà, `StringBuilder` duy trì một bộ đệm để chứa các thêm vào mà không tạo ra các chuỗi mới cho mỗi thao tác nối thêm.

Nội suy chuỗi, được giới thiệu trong C# 6.0, cho phép mã dễ đọc và bảo trì hơn. Nó được biên dịch thành một lời gọi `String.Format()` tại thời điểm biên dịch nhưng dễ nhìn và ít có khả năng sai sót hơn.

Mỗi phương pháp đều có chỗ đứng của mình: nối nhanh (`+`), kết hợp một vài chuỗi (`String.Concat()`), xây dựng chuỗi nặng nề (`StringBuilder`), và chuỗi được định dạng sạch sẽ, mượt mà (nội suy chuỗi).

## Xem thêm

- Tài liệu Microsoft về nối chuỗi: [Nối Chuỗi](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Tài liệu Microsoft về `StringBuilder`: [Lớp StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)

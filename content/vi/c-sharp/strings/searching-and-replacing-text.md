---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:15.417735-07:00
description: "L\xE0m th\u1EBF n\xE0o: C# l\xE0m cho vi\u1EC7c thao t\xE1c v\u0103\
  n b\u1EA3n tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n. D\u01B0\u1EDBi \u0111\
  \xE2y, h\xE3y xem ph\u01B0\u01A1ng th\u1EE9c `string.Replace` \u0111\u1EC3 ho\xE1\
  n \u0111\u1ED5i t\u1EEB."
lastmod: '2024-03-13T22:44:36.637288-06:00'
model: gpt-4-0125-preview
summary: "C# l\xE0m cho vi\u1EC7c thao t\xE1c v\u0103n b\u1EA3n tr\u1EDF n\xEAn kh\xE1\
  \ \u0111\u01A1n gi\u1EA3n."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Làm thế nào:
C# làm cho việc thao tác văn bản trở nên khá đơn giản. Dưới đây, hãy xem phương thức `string.Replace` để hoán đổi từ.

```C#
using System;

public class Program
{
    public static void Main()
    {
        string phrase = "Hello, World!";
        string updatedPhrase = phrase.Replace("World", "C#");
        
        Console.WriteLine(updatedPhrase); // Kết quả: Hello, C#!
    }
}
```

Không phải là khoa học tên lửa, đúng không? Nhưng giả sử chúng ta muốn bỏ qua chữ hoa chữ thường hoặc thay thế chỉ từ nguyên? Regex sẽ giải cứu:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "Apples grow on trees. apple pies are tasty.";
        string pattern = "\\bapple\\b"; // \b là một ranh giới từ trong Regex
        string replacement = "Orange";
        
        string updatedPhrase = Regex.Replace(phrase, pattern, replacement, RegexOptions.IgnoreCase);

        Console.WriteLine(updatedPhrase); // Kết quả: Oranges grow on trees. Orange pies are tasty.
    }
}
```

## Sâu hơn
Ngày xưa, thao tác chuỗi là một rắc rối. C là tất cả những gì chúng ta có, và nó có nghĩa là phải xử lý mảng ký tự và lặp thủ công. C# đã tặng chúng ta một món quà: xử lý chuỗi dễ dàng.

Nếu `string.Replace` hoặc `Regex.Replace` không đáp ứng được, chúng ta có lựa chọn khác. Đối với văn bản lớn hoặc mẫu phức tạp, cân nhắc viết một parser tùy chỉnh hoặc sử dụng thư viện như Antlr.

Regex mạnh mẽ cho khớp mẫu nhưng có thể chậm. Nếu hiệu suất là quan trọng và bạn quan tâm đến chi tiết, đo lường và so sánh với `StringBuilder` cho thay thế lớn, lặp đi lặp lại.

## Xem thêm
- Tài liệu Microsoft về [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace)
- Lớp [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) của .NET cho mẫu phức tạp hơn
- Xem Antlr cho việc phân tích cú pháp phức tạp: [Hướng dẫn Mega của ANTLR](https://tomassetti.me/antlr-mega-tutorial/)

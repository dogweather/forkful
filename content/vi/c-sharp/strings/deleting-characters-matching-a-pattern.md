---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:37.929710-07:00
description: "L\xE0m th\u1EBF n\xE0o: Mu\u1ED1n lo\u1EA1i b\u1ECF m\u1ED9t s\u1ED1\
  \ k\xFD t\u1EF1? D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch th\u1EF1c hi\u1EC7n trong\
  \ C#."
lastmod: '2024-03-13T22:44:36.635838-06:00'
model: gpt-4-0125-preview
summary: "Mu\u1ED1n lo\u1EA1i b\u1ECF m\u1ED9t s\u1ED1 k\xFD t\u1EF1."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Làm thế nào:
Muốn loại bỏ một số ký tự? Dưới đây là cách thực hiện trong C#:

```C#
using System;
using System.Text.RegularExpressions;

class PatternDeletion
{
    static void Main()
    {
        string originalText = "B4n4n4 P1zza!";
        string pattern = @"[0-9]+"; // Loại bỏ tất cả các chữ số
        
        string cleanedText = Regex.Replace(originalText, pattern, string.Empty);
        
        Console.WriteLine(cleanedText); // Xuất ra: Bnnn Pzza!
    }
}
```
Cần cắt 'a' theo sau là một chữ số? Xem ở đây:

```C#
string targetedRemoval = "C4ndy C4ne";
string complexPattern = @"a[0-9]"; // Nhắm vào 'a' theo sau là bất kỳ chữ số nào

string refinedText = Regex.Replace(targetedRemoval, complexPattern, string.Empty);

Console.WriteLine(refinedText); // Xuất ra: Cndy Cne
```

## Sâu hơn
Regex (Biểu thức chính quy) cung cấp khả năng so khớp mẫu, có nguồn gốc lý thuyết từ những năm 1950 (cảm ơn lí thuyết tự động hóa!). Các phương án thay thế cho regex bao gồm `String.Replace()` trực tiếp cho các thay thế đơn giản hơn, hoặc các thuật toán tùy chỉnh nếu hiệu năng là quan trọng (bởi vì regex có một số overhead). Những phương án thay thế này thiếu sự linh hoạt và độ chính xác làm cho regex trở thành lựa chọn cho các mẫu phức tạp. Khi thực hiện xóa mẫu, hãy chú ý đến bản chất hai lưỡi của regex – chúng mạnh mẽ nhưng có thể rối rắm và chậm đối với dữ liệu lớn.

## Xem thêm
- Tài liệu Regex của Microsoft: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- Regex101 (để kiểm thử mẫu regex): https://regex101.com/
- Giới thiệu về Lý thuyết Tự động hóa: https://en.wikipedia.org/wiki/Automata_theory

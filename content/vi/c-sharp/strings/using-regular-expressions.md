---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:12.052119-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c d\xF9ng \u0111\u1EC3 kh\u1EDBp chu\u1ED7i k\xFD t\u1EF1. L\u1EADp\
  \ tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9\
  nh s\u1EEDa ho\u1EB7c x\xE1c th\u1EF1c v\u0103n b\u1EA3n. Ch\xFAng\u2026"
lastmod: 2024-02-19 22:04:55.817027
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c d\xF9ng \u0111\u1EC3 kh\u1EDBp chu\u1ED7i k\xFD t\u1EF1. L\u1EADp\
  \ tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9\
  nh s\u1EEDa ho\u1EB7c x\xE1c th\u1EF1c v\u0103n b\u1EA3n. Ch\xFAng\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Là gì và Tại sao?
Biểu thức chính quy (regex) là những mẫu được dùng để khớp chuỗi ký tự. Lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa hoặc xác thực văn bản. Chúng mạnh mẽ và hiệu quả, cắt qua chuỗi ký tự như dao cắt qua bơ.

## Làm thế nào:
Hãy xem xét việc khớp, thay thế, và tách chuỗi sử dụng regex trong C#.

**Khớp Số Điện Thoại:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string pattern = @"\b\d{3}[-.]?\d{3}[-.]?\d{4}\b";
        string text = "Call me on 123-456-7890 or 987.654.3210.";
        MatchCollection matches = Regex.Matches(text, pattern);

        foreach (Match match in matches)
           Console.WriteLine(match.Value);
    }
}
```

Kết quả:
```
123-456-7890
987.654.3210
```

**Thay Thế Dòng Mới:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "Dòng đầu tiên.\nDòng thứ hai.\nDòng thứ ba.";
        string pattern = @"\n";
        string replacement = " ";

        string result = Regex.Replace(text, pattern, replacement);
        Console.WriteLine(result);
    }
}
```

Kết quả:
```
Dòng đầu tiên. Dòng thứ hai. Dòng thứ ba.
```

**Tách CSV:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "một,hai,ba,bốn";
        string pattern = @",";

        string[] substrings = Regex.Split(text, pattern);
        foreach (string match in substrings)
        {
            Console.WriteLine(match);
        }
    }
}
```

Kết quả:
```
một
hai
ba
bốn
```

## Tìm hiểu sâu
Regex đã có từ những năm 1950, nhờ nhà toán học Stephen Kleene. Các phương pháp thay thế cho regex bao gồm các phương thức chuỗi như `Contains`, `IndexOf`, `StartsWith`, vv., nhưng chúng kém mạnh mẽ hơn cho các mẫu phức tạp.

Nói về triển khai, lớp `Regex` của C# nằm trong `System.Text.RegularExpressions`. Nó sử dụng các thuật toán backtracking cho việc khớp mẫu. Các thao tác Regex có thể tốn kém; sử dụng một cách cẩn thận để tránh ảnh hưởng đến hiệu suất.

## Xem thêm
- [Tài liệu Regex của Microsoft](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Công cụ Kiểm tra & Gỡ lỗi Regex](https://regex101.com/)
- [Làm chủ Biểu thức Chính Quy](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) của Jeffrey Friedl. _Ghi chú từ [Robert](https://forkful.ai/en/about/): đây là cách tôi đã học Regex. Tôi cảm thấy thực sự hiểu chúng sau khi đọc cuốn sách. Và ngày nay, tôi sử dụng "Công cụ Kiểm tra & Gỡ lỗi Regex", được liệt kê ở trên, khi tôi cần gỡ lỗi._

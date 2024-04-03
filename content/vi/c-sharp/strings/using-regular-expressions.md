---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:12.052119-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y xem x\xE9t vi\u1EC7c kh\u1EDBp, thay\
  \ th\u1EBF, v\xE0 t\xE1ch chu\u1ED7i s\u1EED d\u1EE5ng regex trong C#. **Kh\u1EDB\
  p S\u1ED1 \u0110i\u1EC7n Tho\u1EA1i:**."
lastmod: '2024-03-13T22:44:36.644033-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y xem x\xE9t vi\u1EC7c kh\u1EDBp, thay th\u1EBF, v\xE0 t\xE1ch chu\u1ED7\
  i s\u1EED d\u1EE5ng regex trong C#."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

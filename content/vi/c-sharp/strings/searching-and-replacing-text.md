---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:15.417735-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản trong chuỗi giúp bạn cập nhật dữ liệu mà không cần chỉnh sửa thủ công. Lập trình viên cần điều này để xử lý sửa đổi đầu vào của người dùng, định dạng dữ liệu, hoặc cập nhật hàng loạt một cách hiệu quả.

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

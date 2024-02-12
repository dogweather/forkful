---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases: - /vi/c-sharp/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:46.937119-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do và Mục đích

Việc tìm độ dài của chuỗi đề cập đến việc đếm số lượng ký tự của nó. Chúng ta thực hiện điều này để xác nhận đầu vào, lặp qua các ký tự, phân bổ tài nguyên, hoặc chỉ đơn giản là tò mò – biết kích thước là quan trọng.

## Cách thực hiện:

Trong C#, thuộc tính `string.Length` cung cấp số lượng ký tự có trong một chuỗi. Dưới đây là cách sử dụng nó:

```C#
using System;

class Program
{
    static void Main()
    {
        string example = "Hello, World!";
        Console.WriteLine(example.Length); // Output: 13
    }
}
```

Dễ dàng, phải không? Nhưng hãy nhớ, nó đếm *ký tự*, không phải byte. Với các emoji hoặc ký tự đặc biệt, mọi thứ có thể trở nên phức tạp hơn. Chúng ta sẽ nói thêm về điều này sau.

## Sâu hơn nữa

Theo lịch sử, việc tìm độ dài của một chuỗi đã gắn liền với quản lý và thao tác bộ nhớ trong lập trình. Do C# là một ngôn ngữ cấp cao, nó đã trừu tượng hóa công việc cấp thấp đó đi. Tuy nhiên, biết được điều gì đang diễn ra bên dưới cũng rất tốt.

Có phương pháp khác không? Chắc chắn rồi! Bạn có thể thấy `example.ToCharArray().Length` được sử dụng ngoài kia, nhưng nó chỉ thực hiện công việc nặng nhọc để đạt được kết quả giống nhau.

Giờ đây, về những ký tự phức tạp đó. Thuộc tính `Length` của C# đếm số lượng đối tượng `char` trong chuỗi, mỗi đối tượng đại diện cho một đơn vị mã UTF-16. Điều đó ổn cho đến khi bạn gặp *cặp thay thế* – những ký tự như emoji cần hai đối tượng `char`. Dưới đây là vấn đề: `Length` đếm chúng như là hai. Đúng vậy.

Để có số lượng chính xác của *ký tự trực quan* hay *nhóm grapheme*, bạn sẽ cần lớp `StringInfo` từ System.Globalization:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string example = "👍"; // Emoji cử chỉ ok

        Console.WriteLine(example.Length); // Output: 2 <- Do cặp thay thế!
        Console.WriteLine(new StringInfo(example).LengthInTextElements); // Output: 1
    }
}
```

Bạn đã hiểu sự khác biệt chưa? Đây không chỉ là lý thuyết; nó có thể ảnh hưởng đến xử lý văn bản một cách có ý nghĩa.

## Xem thêm

Khám phá thêm với các nguồn này:

- [Tài liệu chính thức của Microsoft về chuỗi](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Hiểu biết về Unicode và UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html)
- [Tài liệu về lớp StringInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=net-6.0)

Hiểu biết về chuỗi, xử lý chúng một cách khôn ngoan, và viết mã đếm - theo mọi nghĩa.

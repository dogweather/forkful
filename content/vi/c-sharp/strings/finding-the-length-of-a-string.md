---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:46.937119-07:00
description: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i \u0111\u1EC1\
  \ c\u1EADp \u0111\u1EBFn vi\u1EC7c \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1\
  \ c\u1EE7a n\xF3. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ x\xE1c nh\u1EADn \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ph\xE2\
  n b\u1ED5 t\xE0i\u2026"
lastmod: 2024-02-19 22:04:55.818434
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i \u0111\u1EC1 c\u1EAD\
  p \u0111\u1EBFn vi\u1EC7c \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 c\u1EE7\
  a n\xF3. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1\
  c nh\u1EADn \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ph\xE2n b\u1ED5\
  \ t\xE0i\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
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

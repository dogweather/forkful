---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:20.966984-07:00
description: "C\xE1ch th\u1EE9c: C# l\xE0m cho vi\u1EC7c r\xFAt tr\xEDch c\xE1c chu\u1ED7\
  i con t\u1EEB m\u1ED9t chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1i nh\xECn nhanh v\u1EC1 c\xE1ch th\u1EF1c hi\u1EC7n s\u1EED\
  \ d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c\u2026"
lastmod: '2024-03-13T22:44:36.642719-06:00'
model: gpt-4-0125-preview
summary: "C# l\xE0m cho vi\u1EC7c r\xFAt tr\xEDch c\xE1c chu\u1ED7i con t\u1EEB m\u1ED9\
  t chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cách thức:
C# làm cho việc rút trích các chuỗi con từ một chuỗi trở nên dễ dàng. Dưới đây là cái nhìn nhanh về cách thực hiện sử dụng phương thức `Substring` và cắt chuỗi với các toán tử phạm vi.

```C#
string fullString = "Hello, World! Life is beautiful.";
// Sử dụng Substring(startIndex, length)
string extracted1 = fullString.Substring(7, 5); // "World"

Console.WriteLine(extracted1); // Kết quả: World

// Sử dụng cắt chuỗi với toán tử phạm vi [..]
string extracted2 = fullString[13..24]; // "Life is beau"

Console.WriteLine(extracted2); // Kết quả: Life is beau
```

## Sâu hơn
Rút trích các chuỗi con không phải là một mẹo mới. Chúng đã có mặt trong các ngôn ngữ như C và Java từ lâu. Tuy nhiên, C# đã tinh chỉnh quá trình này với các phương thức và tính năng ưu tiên khả năng đọc và dễ sử dụng.

Trước đây, lập trình viên sử dụng các vòng lặp và tính toán chỉ mục một cách cẩn thận. Phương thức `Substring` trong C# là một bản nâng cấp tuyệt vời. Nó thẳng thắn—chỉ cần đưa vào chỉ số bắt đầu và, tùy chọn, một độ dài, và nó sẽ thực hiện việc cắt cho bạn.

Không dừng lại ở đó. Với C# 8.0 và sau đó, chúng ta đã được giới thiệu với các toán tử phạm vi như `[..]`. Chúng cho phép biểu thức cắt tự nhiên hơn, đặc biệt khi sử dụng chỉ mục liên quan đến cuối chuỗi (được biểu thị bằng toán tử `^`).

Các phương thức thay thế cho `Substring` bao gồm các phương thức như `Split`, các thao tác Regex, hoặc thao tác chuỗi với LINQ. Sự lựa chọn phụ thuộc vào tình huống—bạn có thể chia một dòng CSV, Regex một mẫu, hoặc rút trích các chuỗi con với một biểu thức LINQ tinh vi.

Về phía triển khai, các chuỗi trong C# là bất biến. Khi bạn lấy một chuỗi con, bạn không thay đổi chuỗi gốc. Thay vào đó, bạn tạo ra một chuỗi mới chia sẽ một số không gian bộ nhớ của chuỗi mẹ — cho đến khi bạn thay đổi nó, và lúc đó nó sẽ chuyển sang phân bổ bộ nhớ của riêng mình.

## Xem thêm
Nếu bạn muốn tìm hiểu sâu hơn hoặc khám phá các chủ đề liên quan, dưới đây là một số tài nguyên:
- Tài liệu chính thức của Microsoft về `Substring`: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- Thêm thông tin về các toán tử phạm vi và chỉ mục trong C#: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-8.0/ranges
- Thao tác chuỗi với LINQ: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/
- Biểu thức chính quy trong C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions

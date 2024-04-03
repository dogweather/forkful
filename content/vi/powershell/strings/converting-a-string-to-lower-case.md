---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:12.626522-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PowerShell kh\xE1 ti\u1EC7n l\u1EE3i v\u1EDB\
  i chu\u1ED7i. S\u1EED d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `.ToLower()`, nh\u01B0\
  \ sau."
lastmod: '2024-03-13T22:44:36.916096-06:00'
model: gpt-4-0125-preview
summary: "PowerShell kh\xE1 ti\u1EC7n l\u1EE3i v\u1EDBi chu\u1ED7i."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thực hiện:
PowerShell khá tiện lợi với chuỗi. Sử dụng phương thức `.ToLower()`, như sau:

```PowerShell
$string = "HELLO, World!"
$lowerCaseString = $string.ToLower()
$lowerCaseString
```

Kết quả đầu ra:

```
hello, world!
```

Hay thử phương thức `ToLowerInvariant()` khi các quy chuẩn văn hóa không nên ảnh hưởng đến việc chuyển đổi:

```PowerShell
$string = "HELLO, World!"
$lowerCaseInvariant = $string.ToLowerInvariant()
$lowerCaseInvariant
```

Kết quả đầu ra:

```
hello, world!
```

## Sâu hơn nữa
Ngày xưa, không phân biệt chữ hoa chữ thường khá phổ biến trong ngôn ngữ lập trình. Trong PowerShell, giống như các tiền bối .NET của nó, chuỗi là các đối tượng với các phương thức tích hợp sẵn để thao tác. Khi chúng ta sử dụng `.ToLower()`, chúng ta đang gọi một phương thức xử lý quá trình chuyển đổi cho chúng ta.

Có cách thay thế để hoàn thành công việc? Chắc chắn rồi. Bạn có thể sử dụng:

- Vòng lặp `for`, thăm từng kí tự, và chuyển đổi chữ hoa thủ công
- Biểu thức chính quy với toán tử `-replace`
- Chuyển đổi cụ thể theo văn hóa sử dụng các phương thức nạp chồng `.ToLower()`

Tại sao sử dụng văn hóa không biến đổi với `ToLowerInvariant()`? Điều đó cần thiết để đạt được kết quả nhất quán trên các địa phương khác nhau, nơi mà sự hiểu biết về cái gì là "chữ thường" có thể khác biệt.

## Xem thêm
Để khám phá chi tiết hơn trong việc thao tác chuỗi, truy cập những liên kết sau:

- [Lớp Chuỗi .NET](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)

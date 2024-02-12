---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/powershell/converting-a-string-to-lower-case/
date:                  2024-01-28T21:58:12.626522-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc chuyển một chuỗi thành chữ thường có nghĩa là biến mọi chữ cái trong chuỗi thành chữ cái nhỏ. Các lập trình viên làm điều này để chuẩn hóa văn bản, để so sánh, và đôi khi tuân theo các quy tắc phân biệt chữ hoa chữ thường trong lập trình hoặc lưu trữ dữ liệu.

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

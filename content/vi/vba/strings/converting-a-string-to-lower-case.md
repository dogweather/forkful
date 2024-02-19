---
aliases:
- /vi/vba/converting-a-string-to-lower-case/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:42.528476-07:00
description: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF\
  \ th\u01B0\u1EDDng bao g\u1ED3m vi\u1EC7c bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3\
  \ c\xE1c k\xFD t\u1EF1 in hoa trong chu\u1ED7i th\xE0nh c\xE1c k\xFD t\u1EF1 ch\u1EEF\
  \ th\u01B0\u1EDDng t\u01B0\u01A1ng \u0111\u01B0\u01A1ng. Qu\xE1 tr\xECnh n\xE0y\u2026"
lastmod: 2024-02-18 23:08:50.494495
model: gpt-4-0125-preview
summary: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF\
  \ th\u01B0\u1EDDng bao g\u1ED3m vi\u1EC7c bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3\
  \ c\xE1c k\xFD t\u1EF1 in hoa trong chu\u1ED7i th\xE0nh c\xE1c k\xFD t\u1EF1 ch\u1EEF\
  \ th\u01B0\u1EDDng t\u01B0\u01A1ng \u0111\u01B0\u01A1ng. Qu\xE1 tr\xECnh n\xE0y\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc chuyển đổi một chuỗi thành chữ thường bao gồm việc biến đổi tất cả các ký tự in hoa trong chuỗi thành các ký tự chữ thường tương đương. Quá trình này rất cần thiết cho nhiều tác vụ lập trình, bao gồm chuẩn hóa dữ liệu, so sánh không phân biệt chữ hoa chữ thường, và cải thiện tính nhất quán của đầu vào từ người dùng.

## Cách thực hiện:

Trong Visual Basic for Applications (VBA), việc chuyển đổi một chuỗi thành chữ thường rất đơn giản sử dụng hàm `LCase`. Hàm này nhận vào một chuỗi và trả về một chuỗi mới với tất cả các ký tự in hoa được chuyển thành chữ thường. Dưới đây là một ví dụ cơ bản để minh họa điều này:

```basic
Dim originalString As String
Dim lowerCaseString As String

originalString = "Hello, World!"
lowerCaseString = LCase(originalString)

Debug.Print lowerCaseString ' Kết quả: hello, world!
```

Bạn cũng có thể sử dụng trực tiếp `LCase` trong các phép so sánh hoặc gán để làm cho mã gọn gàng hơn:

```basic
If LCase(userInput) = "yes" Then
    Debug.Print "Người dùng đã nói có"
End If
```

Ví dụ thứ hai trình bày cách xử lý đầu vào từ người dùng một cách không phân biệt chữ hoa chữ thường bằng cách chuyển đổi đầu vào thành chữ thường trước khi so sánh.

## Sâu hơn nữa

Hàm `LCase` là nền tảng cho việc thao tác chuỗi trong VBA và đã là tính năng cốt lõi kể từ khi ngôn ngữ được tạo ra. Nó đơn giản hóa các tác vụ chuyển đổi chữ thường, thường gặp trong việc phân tích dữ liệu và xử lý đầu vào từ người dùng. Mặc dù `LCase` hiệu quả đáp ứng nhu cầu chuyển đổi ký tự thành chữ thường trong các ứng dụng khác nhau, cũng quan trọng khi nhận biết các hạn chế và các lựa chọn thay thế của nó.

Ví dụ, trong khi `LCase` hoạt động mượt mà cho bảng chữ cái tiếng Anh, việc xử lý ngôn ngữ có quy tắc chữ hoa phức tạp hơn có thể yêu cầu các xem xét bổ sung hoặc sử dụng hàm `StrConv` với các cài đặt địa phương phù hợp cho việc chuyển đổi chữ hoa.

Hơn nữa, khi chuyển từ các ngôn ngữ như Python, nơi sử dụng `str.lower()`, hoặc JavaScript, với `string.toLowerCase()` của nó, các lập trình viên có thể thấy `LCase` đơn giản nhưng nên nhớ về các điểm đặc trưng của VBA, như việc thiếu khả năng kết nối phương thức.

Tóm lại, mặc dù có các lựa chọn mới và có thể mạnh mẽ hơn trong các ngôn ngữ khác, `LCase` vẫn là một hàm đáng tin cậy và dễ sử dụng để chuyển đổi chuỗi thành chữ thường trong VBA, phù hợp tốt với cú pháp tổng thể và lược đồ chức năng của ngôn ngữ.

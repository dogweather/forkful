---
title:                "Sử dụng biểu thức chính quy"
aliases:
- /vi/vba/using-regular-expressions/
date:                  2024-02-01T22:05:13.483732-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Biểu thức chính quy (regex) trong Visual Basic for Applications (VBA) cung cấp một cách mạnh mẽ để tìm kiếm, so khớp và thao tác với chuỗi. Các lập trình viên sử dụng chúng cho các công việc như xác thực dữ liệu, phân tích cú pháp và biến đổi vì sự linh hoạt và hiệu quả của nó trong việc xử lý các mẫu chuỗi phức tạp.

## Làm thế nào:

Để sử dụng biểu thức chính quy trong VBA, trước tiên bạn cần phải kích hoạt thư viện Microsoft VBScript Regular Expressions. Trong trình biên soạn VBA, đi tới `Tools` -> `References`, sau đó đánh dấu kiểm `Microsoft VBScript Regular Expressions 5.5`.

Dưới đây là một ví dụ cơ bản để tìm kiếm một mẫu trong chuỗi:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' Tìm từ "is"
    End With

    Dim testString As String
    testString = "This is a test string."

    If regex.Test(testString) Then
        MsgBox "Pattern found."
    Else
        MsgBox "Pattern not found."
    End If
End Sub
```

Để thay thế một mẫu trong chuỗi:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' Đối sánh bất kỳ ký tự khoảng trắng nào
    End With

    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' Xuất ra: "This_is_a_test_string."
End Sub
```

## Sâu hơn nữa

Việc bao gồm biểu thức chính quy trong các ngôn ngữ lập trình thường trở lại với các công cụ Unix từ những năm 1970. VBA tích hợp regex thông qua thư viện Regular Expressions của VBScript, làm nổi bật tầm quan trọng của nó trong các tác vụ xử lý văn bản ngay cả trong các ứng dụng không thường được liên kết với việc thao tác văn bản nặng như Excel hay Access.

Mặc dù mạnh mẽ, regex trong VBA đôi khi có thể kém trực quan hoặc hiệu suất so với các triển khai hiện đại hơn trong các ngôn ngữ như Python hay JavaScript. Ví dụ, mô-đun `re` của Python cung cấp hỗ trợ rộng lớn cho các nhóm được đặt tên và các tính năng khớp mẫu tinh vi hơn, mang lại cách tiếp cận sạch sẽ và có thể dễ đọc hơn. Tuy nhiên, khi làm việc trong hệ sinh thái VBA, biểu thức chính quy vẫn là một công cụ không thể thiếu cho các tác vụ đòi hỏi so khớp mẫu hoặc thao tác văn bản. Sự đánh đổi về hiệu suất thường không đáng kể so với sự tiện lợi và khả năng mà regex mang lại khi xử lý chuỗi trong các ứng dụng Office.

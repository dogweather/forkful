---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:09.022887-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: VBA kh\xF4ng h\u1ED7 tr\u1EE3 ph\xE2n t\xED\
  ch c\xFA ph\xE1p JSON m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn, v\xEC v\u1EADy ch\xFAng\
  \ ta s\u1EBD s\u1EED d\u1EE5ng ng\xF4n ng\u1EEF k\u1ECBch b\u1EA3n nh\u01B0 JScript\
  \ (th\xF4ng qua \u0111\u1ED1i t\u01B0\u1EE3ng\u2026"
lastmod: '2024-03-13T22:44:36.461454-06:00'
model: gpt-4-0125-preview
summary: "VBA kh\xF4ng h\u1ED7 tr\u1EE3 ph\xE2n t\xEDch c\xFA ph\xE1p JSON m\u1ED9\
  t c\xE1ch t\u1EF1 nhi\xEAn, v\xEC v\u1EADy ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng\
  \ ng\xF4n ng\u1EEF k\u1ECBch b\u1EA3n nh\u01B0 JScript (th\xF4ng qua \u0111\u1ED1\
  i t\u01B0\u1EE3ng ScriptControl) \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p chu\u1ED7\
  i JSON v\xE0 x\xE2y d\u1EF1ng \u0111\u1ED1i t\u01B0\u1EE3ng JSON."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cách thực hiện:
VBA không hỗ trợ phân tích cú pháp JSON một cách tự nhiên, vì vậy chúng ta sẽ sử dụng ngôn ngữ kịch bản như JScript (thông qua đối tượng ScriptControl) để phân tích cú pháp chuỗi JSON và xây dựng đối tượng JSON. Dưới đây là cách bạn có thể phân tích cú pháp một chuỗi JSON trong VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Tên: " & parsed.name & ", Tuổi: " & parsed.age & ", Thành phố: " & parsed.city
End Sub
```

Để tạo JSON, bạn có thể sử dụng một cách tiếp cận tương tự, xây dựng chuỗi JSON thông qua việc nối chuỗi:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Sâu hơn
Các phương pháp được trình bày tận dụng ScriptControl để xử lý JSON, cơ bản là ủy thác công việc cho một động cơ JavaScript. Đây là một giải pháp sáng tạo nhưng không nhất thiết là cách hiệu quả hoặc hiện đại nhất để làm việc với JSON trong bối cảnh VBA. Trong các ứng dụng phức tạp hơn, phương pháp này có thể trở nên cồng kềnh và mang lại gánh nặng về hiệu suất hoặc những lo ngại về bảo mật, vì ScriptControl thực thi trong một môi trường có quyền truy cập đầy đủ vào máy tính chủ.

Các môi trường lập trình khác, như Python hoặc JavaScript, cung cấp hỗ trợ sẵn có cho JSON, làm cho chúng phù hợp hơn cho các ứng dụng cần thao tác JSON một cách rộng rãi. Những ngôn ngữ này cung cấp thư viện toàn diện hỗ trợ không chỉ phân tích cú pháp và tạo ra mà còn truy vấn và định dạng dữ liệu JSON.

Mặc dù VBA có những hạn chế này, việc hiểu cách làm việc với JSON là rất quan trọng trong thế giới nơi mà giao tiếp dữ liệu dựa trên web và các tệp cấu hình chủ yếu được định dạng bằng JSON. Đối với các lập trình viên VBA, việc nắm vững những kỹ thuật này mở ra cơ hội tích hợp với các web APIs, diễn giải các tệp cấu hình, hoặc thậm chí xây dựng ứng dụng web đơn giản. Tuy nhiên, khi dự án phát triển phức tạp hơn hoặc yêu cầu hiệu suất cao hơn, các nhà phát triển có thể xem xét tận dụng các môi trường lập trình thân thiện với JSON hơn.

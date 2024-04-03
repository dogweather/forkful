---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:56.105209-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kh\xE1c v\u1EDBi nh\u1EEFng m\xF4i tr\u01B0\u1EDD\
  ng l\u1EADp tr\xECnh \u0111\u01A1n gi\u1EA3n h\u01A1n, VBA kh\xF4ng c\xF3 t\xED\
  nh n\u0103ng t\xEDch h\u1EE3p \u0111\u1EC3 \u0111\u1ECDc tr\u1EF1c ti\u1EBFp c\xE1\
  c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh m\u1ED9t c\xE1ch th\xF4ng th\u01B0\u1EDD\
  ng\u2026"
lastmod: '2024-03-13T22:44:36.453591-06:00'
model: gpt-4-0125-preview
summary: "Kh\xE1c v\u1EDBi nh\u1EEFng m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh \u0111\
  \u01A1n gi\u1EA3n h\u01A1n, VBA kh\xF4ng c\xF3 t\xEDnh n\u0103ng t\xEDch h\u1EE3\
  p \u0111\u1EC3 \u0111\u1ECDc tr\u1EF1c ti\u1EBFp c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2\
  ng l\u1EC7nh m\u1ED9t c\xE1ch th\xF4ng th\u01B0\u1EDDng b\u1EDFi v\xEC n\xF3 ch\u1EE7\
  \ y\u1EBFu \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF \u0111\u1EC3 nh\xFAng trong c\xE1\
  c \u1EE9ng d\u1EE5ng Microsoft Office."
title: "\u0110\u1ECDc \u0111\u1ED1i s\u1ED1 t\u1EEB d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Khác với những môi trường lập trình đơn giản hơn, VBA không có tính năng tích hợp để đọc trực tiếp các đối số dòng lệnh một cách thông thường bởi vì nó chủ yếu được thiết kế để nhúng trong các ứng dụng Microsoft Office. Tuy nhiên, với một chút sáng tạo, chúng ta có thể sử dụng Windows Script Host (WSH) hoặc gọi các API bên ngoài để đạt được tính năng tương tự. Dưới đây là một giải pháp thực tế sử dụng WSH:

1. **Tạo một VBScript để Truyền Đối Số đến VBA:**

   Đầu tiên, viết một tệp VBScript (*yourScript.vbs*) để khởi chạy ứng dụng VBA của bạn (ví dụ: một macro Excel) và truyền các đối số dòng lệnh:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Truy cập các Đối Số trong VBA:**

   Trong ứng dụng VBA của bạn (*YourMacroWorkbook.xlsm*), chỉnh sửa hoặc tạo macro (*YourMacroName*) để chấp nhận tham số:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Đối số 1: " & arg1 & " Đối số 2: " & arg2
End Sub
```

3. **Chạy Script của Bạn:**

   Thực thi VBScript từ dòng lệnh, truyền đối số theo nhu cầu:

```shell
cscript yourScript.vbs "Xin chào" "Thế giới"
```

   Điều này nên dẫn đến việc macro VBA của bạn được thực thi với các đối số "Xin chào" và "Thế giới", hiển thị chúng trong một hộp thông báo.

## Tìm hiểu sâu hơn:
Trong bối cảnh lịch sử, VBA được thiết kế để mở rộng khả năng của các ứng dụng Microsoft Office, không phải là một môi trường lập trình độc lập. Do đó, tương tác trực tiếp với dòng lệnh nằm ngoài phạm vi chính của nó, điều này giải thích cho việc thiếu hỗ trợ tích hợp sẵn để đọc các đối số dòng lệnh.

Phương pháp được trình bày ở trên, mặc dù hiệu quả, nhưng hơn là một giải pháp tạm thời hơn là một giải pháp bản địa, sử dụng kịch bản bên ngoài để tạo cầu nối. Cách tiếp cận này có thể giới thiệu sự phức tạp và các vấn đề về bảo mật tiềm ẩn do nó yêu cầu kích hoạt macro và có thể hạ thấp cài đặt bảo mật để thực thi.

Đối với các tác vụ phụ thuộc nhiều vào các đối số dòng lệnh hoặc cần tích hợp mượt mà hơn với hệ điều hành Windows, các ngôn ngữ lập trình khác như PowerShell hoặc Python có thể cung cấp các giải pháp mạnh mẽ và an toàn hơn. Những lựa chọn thay thế này cung cấp hỗ trợ trực tiếp cho các đối số dòng lệnh và phù hợp hơn cho các ứng dụng hoặc script độc lập cần đầu vào bên ngoài để thay đổi hành vi một cách động.

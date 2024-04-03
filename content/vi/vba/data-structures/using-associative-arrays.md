---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:58.537789-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong VBA, \u0111\u1ED1i t\u01B0\u1EE3ng `Dictionary`\
  \ cung c\u1EA5p ch\u1EE9c n\u0103ng t\u01B0\u01A1ng t\u1EF1 nh\u01B0 c\xE1c m\u1EA3\
  ng k\u1EBFt h\u1EE3p. B\u1EA1n c\u1EA7n ph\u1EA3i th\xEAm m\u1ED9t tham chi\u1EBF\
  u \u0111\u1EBFn Microsoft Scripting\u2026"
lastmod: '2024-03-13T22:44:36.423164-06:00'
model: gpt-4-0125-preview
summary: "Trong VBA, \u0111\u1ED1i t\u01B0\u1EE3ng `Dictionary` cung c\u1EA5p ch\u1EE9\
  c n\u0103ng t\u01B0\u01A1ng t\u1EF1 nh\u01B0 c\xE1c m\u1EA3ng k\u1EBFt h\u1EE3p."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Trong VBA, đối tượng `Dictionary` cung cấp chức năng tương tự như các mảng kết hợp. Bạn cần phải thêm một tham chiếu đến Microsoft Scripting Runtime để sử dụng nó:

1. Trong trình chỉnh sửa VBA, đi tới Tools > References...
2. Đánh dấu "Microsoft Scripting Runtime" và click OK.

Dưới đây là cách khai báo, điền dữ liệu, và truy cập các phần tử trong `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Thêm phần tử
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Truy cập phần tử
Debug.Print sampleDictionary.Item("Name")  ' Kết quả: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Kết quả: 29

' Kiểm tra nếu một khóa tồn tại
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' Xóa phần tử
sampleDictionary.Remove("Occupation")

' Lặp qua từ điển
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Sâu hơn nữa
Đối tượng `Dictionary` tương tác với các thành phần của Windows Scripting Host phía dưới. Do đó, nó là một đối tượng COM kết nối muộn, đây là một cách phổ biến để mở rộng chức năng của VBA trong quá khứ. Việc sử dụng nó trong VBA có thể cải thiện đáng kể khả năng của ngôn ngữ này trong việc thao tác với các bộ dữ liệu phức tạp mà không cần áp đặt một cấu trúc cứng nhắc, như đã thấy trong các mảng truyền thống hay các dải Excel.

Một hạn chế cần lưu ý là truy cập vào `Dictionary` yêu cầu việc thiết lập một tham chiếu đến Microsoft Scripting Runtime, điều này có thể làm phức tạp việc phân phối các dự án VBA của bạn. Các lựa chọn khác như Collections tồn tại trong VBA nhưng thiếu một số tính năng quan trọng của `Dictionary`, như khả năng kiểm tra dễ dàng sự tồn tại của một khóa mà không gây ra lỗi.

Trong các ngữ cảnh lập trình gần đây hơn, như ngôn ngữ Python cung cấp hỗ trợ tích hợp cho các mảng kết hợp (cũng được gọi là từ điển trong Python) mà không cần thêm tham chiếu bên ngoài. Hỗ trợ tích hợp này đơn giản hóa quy trình và cung cấp nhiều tính năng tiên tiến ngay từ đầu. Tuy nhiên, trong khuôn khổ của VBA và cho các ứng dụng cụ thể nhằm tự động hóa các tác vụ trong bộ Microsoft Office, việc sử dụng đối tượng `Dictionary` vẫn là một phương pháp mạnh mẽ và liên quan để các cấu trúc dữ liệu giống như mảng kết hợp.

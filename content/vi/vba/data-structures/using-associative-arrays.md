---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:58.537789-07:00
description: "C\xE1c m\u1EA3ng k\u1EBFt h\u1EE3p, th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c bi\u1EBFt \u0111\u1EBFn nh\u01B0 b\u1ED9 t\u1EEB \u0111i\u1EC3n trong Visual Basic\
  \ cho \u1EE8ng d\u1EE5ng (VBA), cho ph\xE9p c\xE1c l\u1EADp tr\xECnh vi\xEAn t\u1EA1\
  o ra c\xE1c b\u1ED9 s\u01B0u t\u1EADp c\u1EE7a c\xE1c c\u1EB7p\u2026"
lastmod: '2024-02-25T18:49:34.760032-07:00'
model: gpt-4-0125-preview
summary: "C\xE1c m\u1EA3ng k\u1EBFt h\u1EE3p, th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c bi\u1EBFt \u0111\u1EBFn nh\u01B0 b\u1ED9 t\u1EEB \u0111i\u1EC3n trong Visual Basic\
  \ cho \u1EE8ng d\u1EE5ng (VBA), cho ph\xE9p c\xE1c l\u1EADp tr\xECnh vi\xEAn t\u1EA1\
  o ra c\xE1c b\u1ED9 s\u01B0u t\u1EADp c\u1EE7a c\xE1c c\u1EB7p\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Các mảng kết hợp, thường được biết đến như bộ từ điển trong Visual Basic cho Ứng dụng (VBA), cho phép các lập trình viên tạo ra các bộ sưu tập của các cặp khóa-giá trị. Tính năng này rất quan trọng cho việc lưu trữ và truy xuất dữ liệu hiệu quả, cung cấp một cách quản lý dữ liệu linh hoạt và trực quan hơn so với các chỉ số mảng truyền thống.

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

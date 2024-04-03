---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:32.879641-07:00
description: "L\xE0m th\u1EBF n\xE0o: Xem x\xE9t m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3\
  n trong Visual Basic for Applications (VBA) n\u01A1i ch\xFAng ta c\xF3 m\u1ED9t\
  \ h\xE0m con in ra chi ti\u1EBFt c\u1EE7a m\u1ED9t nh\xE2n vi\xEAn. Ban \u0111\u1EA7\
  u, m\xE3\u2026"
lastmod: '2024-03-13T22:44:36.444200-06:00'
model: gpt-4-0125-preview
summary: "Xem x\xE9t m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3n trong Visual Basic for\
  \ Applications (VBA) n\u01A1i ch\xFAng ta c\xF3 m\u1ED9t h\xE0m con in ra chi ti\u1EBF\
  t c\u1EE7a m\u1ED9t nh\xE2n vi\xEAn."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3 ngu\u1ED3n"
weight: 19
---

## Làm thế nào:
Xem xét một ví dụ cơ bản trong Visual Basic for Applications (VBA) nơi chúng ta có một hàm con in ra chi tiết của một nhân viên. Ban đầu, mã nguồn bị cluttered, khó duy trì hoặc mở rộng.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

Bước tái cấu trúc 1: Trích xuất phương thức. Một trong những kỹ thuật tái cấu trúc phổ biến nhất là lấy một phần cụ thể của mã nguồn và chuyển nó vào phương thức của chính nó. Điều này làm cho mã nguồn trở nên modular hơn và dễ hiểu hơn.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

Bước tái cấu trúc 2: Sử dụng một cấu trúc. Bước này liên quan đến việc sử dụng một cấu trúc dữ liệu để giữ các dữ liệu liên quan, cải thiện tính rõ ràng của mã và làm cho việc truyền dữ liệu nhóm dễ dàng hơn.

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

Những bước này biến đổi mã nguồn rối rắm thành mã nguồn modular, có cấu trúc, cải thiện đáng kể tính dễ đọc và dễ bảo trì.

## Sâu hơn
Khái niệm về tái cấu trúc cũ kỹ như chính lập trình, nhưng cuốn sách của Martin Fowler "Refactoring: Improving the Design of Existing Code" đã đưa nó vào dòng chính, nhấn mạnh tầm quan trọng của nó trong quy trình phát triển phần mềm. Trong Visual Basic for Applications, việc tái cấu trúc có thể hơi khó khăn hơn do thiếu công cụ tích hợp sẵn tìm thấy trong các môi trường phát triển tích hợp (IDEs) hiện đại hỗ trợ tái cấu trúc tự động.

Tuy nhiên, điều này không làm giảm tầm quan trọng của nó. Ngay cả trong VBA, việc áp dụng các kỹ thuật tái cấu trúc cơ bản bằng tay cũng có thể làm tăng đáng kể cơ sở mã, làm cho nó sạch hơn và hiệu quả hơn. Mặc dù VBA có thể không có những tiện nghi hiện đại tương tự, nhưng các nguyên tắc của thiết kế mã tốt vẫn có tính toàn cầu. Các nhà phát triển đến từ các ngôn ngữ khác có thể thấy quá trình thủ công này mệt mõi nhưng chắc chắn sẽ đánh giá cao lợi ích của việc đầu tư thời gian vào việc cải thiện chất lượng mã ngay từ đầu.

Đối với các môi trường phát triển mạnh mẽ hơn hoặc khi làm việc trên các dự án đặc biệt phức tạp, có thể xem xét khám phá các lựa chọn thay thế cung cấp công cụ tái cấu trúc mạnh mẽ hơn hoặc chuyển đổi các dự án VBA sang một ngôn ngữ .NET nơi Visual Studio cung cấp hỗ trợ tái cấu trúc rộng rãi. Dẫu vậy, việc hiểu và áp dụng các nguyên tắc tái cấu trúc trong VBA là một kỹ năng quý giá, nhấn mạnh tầm quan trọng của việc viết mã sạch, dễ bảo trì, bất kể môi trường.

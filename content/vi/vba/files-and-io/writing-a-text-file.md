---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:54.949225-07:00
description: "L\xE0m th\u1EBF n\xE0o: VBA cung c\u1EA5p m\u1ED9t s\u1ED1 ph\u01B0\u01A1\
  ng ph\xE1p \u0111\u1EC3 vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p, nh\u01B0ng m\u1ED9t\
  \ trong nh\u1EEFng c\xE1ch \u0111\u01A1n gi\u1EA3n nh\u1EA5t l\xE0 s\u1EED d\u1EE5\
  ng `FileSystemObject`. D\u01B0\u1EDBi \u0111\xE2y l\xE0\u2026"
lastmod: '2024-03-13T22:44:36.457508-06:00'
model: gpt-4-0125-preview
summary: "VBA cung c\u1EA5p m\u1ED9t s\u1ED1 ph\u01B0\u01A1ng ph\xE1p \u0111\u1EC3\
  \ vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p, nh\u01B0ng m\u1ED9t trong nh\u1EEFng c\xE1\
  ch \u0111\u01A1n gi\u1EA3n nh\u1EA5t l\xE0 s\u1EED d\u1EE5ng `FileSystemObject`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
VBA cung cấp một số phương pháp để viết vào một tệp, nhưng một trong những cách đơn giản nhất là sử dụng `FileSystemObject`. Dưới đây là hướng dẫn từng bước để tạo một tệp văn bản đơn giản và viết dữ liệu vào đó:

1. **Tham chiếu Microsoft Scripting Runtime**: Đầu tiên, đảm bảo rằng trình soạn thảo VBA của bạn có quyền truy cập vào `FileSystemObject`. Vào Tools > References trong trình soạn thảo VBA và kiểm tra "Microsoft Scripting Runtime."

2. **Tạo một Tệp Văn Bản**: Đoạn mã VBA sau đây mô tả cách tạo một tệp văn bản và viết một dòng văn bản vào đó.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' Tham số CreateTextFile: (Tên tệp, Ghi đè, Unicode)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' Viết một dòng văn bản
    textFile.WriteLine "Chào bạn, VBA!"
    
    ' Đóng tệp
    textFile.Close
End Sub
```

Kịch bản này tạo (hoặc ghi đè nếu đã tồn tại) một tệp có tên `example.txt` trong thư mục đã chỉ định và ghi "Chào bạn, VBA!" vào đó trước khi đóng tệp để lưu các thay đổi.

3. **Kết quả Mẫu**:

Sau khi chạy kịch bản VBA trên, bạn sẽ tìm thấy một tệp có tên `example.txt` với nội dung sau:

```
Chào bạn, VBA!
```

## Sâu hơn:
`FileSystemObject` (FSO), một phần của thư viện Microsoft Scripting Runtime, cung cấp một bộ thuộc tính và phương thức phong phú cho các hoạt động với tệp, mở rộng hơn những gì VBA cung cấp truyền thống (ví dụ: `Open`, `Print` #, `Write` #). Ngoài xử lý tệp, FSO còn có thể thao tác với thư mục và ổ đĩa, làm cho nó trở thành một công cụ mạnh mẽ cho các hoạt động hệ thống tệp trong VBA.

Tuy nhiên, đáng lưu ý là trong khi FSO đưa ra một cách tiếp cận hiện đại hơn đối với các thao tác tệp trong VBA, nó có thể tạo ra gánh nặng cho các nhiệm vụ đơn giản so với các câu lệnh xử lý tệp bản địa của VBA. Hơn nữa, vì FSO là một phần của một thư viện bên ngoài, khả năng di động và tương thích với các hệ thống khác (ví dụ, các phiên bản Office cũ hơn, Office Mac) có thể là những mối quan tâm.

Trong những ngữ cảnh mà hiệu suất, khả năng tương thích, hoặc sự phụ thuộc tối thiểu vào bên ngoài là quan trọng, lập trình viên có thể cân nhắc sử dụng các kỹ thuật xử lý tệp tích hợp sẵn của VBA. Tuy nhiên, đối với các thao tác phức tạp hơn hoặc khi làm việc trong một môi trường mà những mối quan tâm này được giảm bớt (như một cài đặt doanh nghiệp kiểm soát), những lợi ích của FileSystemObject thường vượt trội hơn nhược điểm của nó.

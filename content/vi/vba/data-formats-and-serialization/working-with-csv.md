---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:00.523510-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi t\u1EC7p CSV (Comma Separated Values - Gi\xE1\
  \ tr\u1ECB Ph\xE2n c\xE1ch b\u1EB1ng D\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c\
  \ \u0111\u1ECDc t\u1EEB ho\u1EB7c ghi v\xE0o t\u1EC7p v\u0103n b\u1EA3n thu\u1EA7\
  n t\xFAy n\u01A1i c\xE1c tr\u01B0\u1EDDng d\u1EEF\u2026"
lastmod: '2024-03-11T00:14:09.726322-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi t\u1EC7p CSV (Comma Separated Values - Gi\xE1\
  \ tr\u1ECB Ph\xE2n c\xE1ch b\u1EB1ng D\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c\
  \ \u0111\u1ECDc t\u1EEB ho\u1EB7c ghi v\xE0o t\u1EC7p v\u0103n b\u1EA3n thu\u1EA7\
  n t\xFAy n\u01A1i c\xE1c tr\u01B0\u1EDDng d\u1EEF\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Làm việc với tệp CSV (Comma Separated Values - Giá trị Phân cách bằng Dấu phẩy) bao gồm việc đọc từ hoặc ghi vào tệp văn bản thuần túy nơi các trường dữ liệu được phân cách bởi dấu phẩy. Các lập trình viên thường thực hiện nhiệm vụ này để tạo điều kiện trao đổi dữ liệu giữa các ứng dụng phần mềm khác nhau, nhờ vào sự đơn giản và sự áp dụng rộng rãi của định dạng CSV trong các môi trường lập trình khác nhau.

## Làm thế nào:

Visual Basic for Applications (VBA) đơn giản hóa việc làm việc với tệp CSV thông qua các hàm và phương thức tích hợp cho phép đọc từ và ghi vào các tệp này một cách liền mạch. Dưới đây là các ví dụ minh họa các thao tác cơ bản với tệp CSV.

### Đọc một Tệp CSV:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Xử lý mảng dataFields theo nhu cầu
        Debug.Print Join(dataFields, ";") 'Ví dụ đầu ra cho thấy sự chuyển đổi từ dấu phẩy thành dấu chấm phẩy
    Loop
    
    Close #1
End Sub
```

### Ghi vào một Tệp CSV:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Tên,Tuổi" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Mở filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Dữ liệu Mẫu trong `output.csv`:
```
ID,Tên,Tuổi
1,John Doe,30
2,Jane Doe,29
```

## Sâu hơn nữa

Trong lịch sử, tệp CSV đã là một phương pháp đơn giản để lưu trữ dữ liệu dạng bảng trong định dạng văn bản. Sự đơn giản của cấu trúc của nó, nơi mỗi dòng tương ứng với một bản ghi dữ liệu và mỗi trường trong một bản ghi được phân cách bởi một dấu phẩy, là điểm mạnh và cũng là giới hạn của CSV. Định dạng này không hỗ trợ natively các kiểu dữ liệu, điều này có nghĩa tất cả dữ liệu được lưu trữ dưới dạng chuỗi, và gánh nặng chuyển đổi dữ liệu sang loại đúng nằm trên vai của lập trình viên.

Trong Visual Basic for Applications, việc xử lý tệp CSV chủ yếu được thực hiện thông qua các thao tác tệp cơ bản, như được minh họa trong các ví dụ trước đó. Không có sự hỗ trợ phân tích cú pháp CSV trực tiếp như trong các ngôn ngữ hiện đại hơn (ví dụ, mô-đun csv của Python), điều này cung cấp nhiều kiểm soát và tiện lợi hơn khi xử lý dữ liệu CSV.

Đối với các thao tác phức tạp hơn hoặc khi làm việc với các tệp CSV lớn, các lập trình viên có thể tìm thấy những lựa chọn tốt hơn ngoài VBA thuần túy, chẳng hạn như tận dụng các thư viện bên ngoài hoặc sử dụng các ngôn ngữ lập trình khác được trang bị khả năng xử lý CSV tinh vi hơn. Tuy nhiên, đối với các nhiệm vụ đơn giản liên quan đến tệp CSV, phương pháp tiếp cận trực tiếp của VBA thường đủ và dễ thực hiện, cung cấp một giải pháp nhanh chóng cho các ứng dụng dựa trên Excel hoặc tự động hóa phần mềm Microsoft Office khác.

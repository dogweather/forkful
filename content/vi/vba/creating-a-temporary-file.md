---
title:                "Tạo một tệp tạm thời"
aliases:
- vi/vba/creating-a-temporary-file.md
date:                  2024-02-01T21:52:34.445603-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tệp tạm thời"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/creating-a-temporary-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo một tệp tạm thời trong Visual Basic for Applications (VBA) bao gồm việc tạo ra một tệp thông qua lập trình dùng cho mục đích ngắn hạn, thường là cho việc xử lý dữ liệu hoặc như một bộ đệm trong các tác vụ tự động hóa. Lập trình viên thực hiện điều này để quản lý dữ liệu không cần được lưu trữ lâu dài, giảm thiểu rác và đảm bảo hiệu quả trong việc sử dụng bộ nhớ.

## Làm thế nào:

Trong VBA, việc tạo một tệp tạm thời có thể thực hiện được thông qua việc sử dụng `FileSystemObject`, có sẵn trong thư viện Microsoft Scripting Runtime. Đối tượng này cung cấp các phương thức để tạo, đọc, viết và xóa các tệp và thư mục. Dưới đây là hướng dẫn từng bước để tạo một tệp tạm thời:

1. **Kích hoạt Microsoft Scripting Runtime**: Trước tiên, hãy chắc chắn rằng tham chiếu đến Microsoft Scripting Runtime đã được kích hoạt trong môi trường VBA của bạn. Đi tới Tools > References trong trình chỉnh sửa VBA, và kiểm tra "Microsoft Scripting Runtime".

2. **Tạo một Tệp Tạm thời**: Đoạn mã VBA sau đây minh họa việc tạo một tệp tạm thời trong thư mục tạm thời mặc định.

```vb
Sub CreateTemporaryFile()
    Dim fso As Object
    Dim tmpFile As Object
    
    ' Tạo FileSystemObject
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    ' Lấy đường dẫn của thư mục tạm thời
    Dim tempFolder As String
    tempFolder = fso.GetSpecialFolder(2) ' 2 chỉ thư mục tạm thời
    
    ' Tạo một tệp tạm thời và nhận một tham chiếu tới nó
    Set tmpFile = fso.CreateTextFile(tempFolder & "\myTempFile.txt", True)
    
    ' Viết gì đó vào tệp
    tmpFile.WriteLine "Đây là một kiểm tra."
    
    ' Đóng tệp
    tmpFile.Close
    
    ' Tùy chọn, in đường dẫn để tham khảo
    Debug.Print "Tạo tệp tạm thời tại: " & tempFolder & "\myTempFile.txt"
End Sub
```

3. **Đầu ra Mẫu**: Khi bạn chạy mã trên, nó tạo một tệp tạm thời có tên `myTempFile.txt` trong thư mục tạm thời và viết một dòng văn bản vào đó. Nếu bạn có cửa sổ Immediate mở (`Ctrl + G` trong trình chỉnh sửa VBA), bạn sẽ thấy:
   
```
Tạo tệp tạm thời tại: C:\Users\[YourUsername]\AppData\Local\Temp\myTempFile.txt
```

## Đào Sâu

Phương pháp được trình bày sử dụng `FileSystemObject` (FSO), một phần của Microsoft Scripting Runtime. FSO là một công cụ mạnh mẽ cho việc thao tác hệ thống tệp, được giới thiệu cùng với Visual Basic Scripting Edition. Mặc dù đã có tuổi, nhưng nó vẫn được sử dụng rộng rãi trong VBA vì sự đơn giản và phạm vi chức năng rộng lớn của nó.

Việc tạo tệp tạm thời đóng một vai trò quan trọng trong nhiều tác vụ lập trình và kịch bản, cung cấp một hộp cát để thử nghiệm hoặc một không gian làm việc cho các quá trình không yêu cầu lưu trữ vĩnh viễn. Tuy nhiên, các nhà phát triển nên xử lý những tệp này một cách cẩn thận, đảm bảo chúng được loại bỏ hoặc xóa sạch khi không còn cần thiết, để tránh rò rỉ dữ liệu không mong muốn hoặc tiêu thụ không cần thiết dung lượng đĩa.

Mặc dù VBA cung cấp các phương thức gốc cho việc xử lý tệp và thư mục, `FileSystemObject` cung cấp một cách tiếp cận có tính hướng đối tượng hơn, có thể quen thuộc hơn đối với các lập trình viên đến từ các ngôn ngữ khác. Tuy nhiên, các công nghệ hoặc ngôn ngữ mới hơn có thể cung cấp các phương thức mạnh mẽ hoặc an toàn hơn cho việc xử lý tệp tạm thời, chẳng hạn như sử dụng các cấu trúc dữ liệu trong bộ nhớ hoặc thư viện tệp tạm thời chuyên dụng trong môi trường như Python hoặc .NET. Trong những trường hợp này, mặc dù VBA có thể phục vụ tốt cho các nhiệm vụ nhanh chóng hoặc tích hợp trong ứng dụng Office, việc khám phá các phương án thay thế cho các ứng dụng quy mô lớn hơn hoặc nhạy cảm với bảo mật là điều khuyến khích.

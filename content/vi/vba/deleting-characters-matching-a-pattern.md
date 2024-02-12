---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases:
- vi/vba/deleting-characters-matching-a-pattern.md
date:                  2024-02-01T21:54:18.895797-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xóa ký tự phù hợp với một mẫu cụ thể trong Visual Basic cho Applications (VBA) bao gồm việc xác định và sau đó loại bỏ các ký tự hoặc chuỗi phù hợp với tiêu chí nhất định. Thao tác này phổ biến trong các nhiệm vụ làm sạch và định dạng dữ liệu, nơi loại bỏ các ký tự không cần thiết hoặc không mong muốn khỏi các chuỗi là thiết yếu để duy trì tính toàn vẹn dữ liệu và tạo điều kiện cho việc xử lý dữ liệu tiếp theo.

## Làm thế nào:

Trong VBA, bạn có thể sử dụng hàm `Replace` hoặc biểu thức chính quy để xóa ký tự phù hợp với một mẫu. Dưới đây là các ví dụ về cả hai phương pháp:

### Sử dụng Hàm `Replace`

Hàm `Replace` đơn giản để loại bỏ các ký tự cụ thể hoặc chuỗi.

```basic
Sub DeleteSpecificChars()
    Dim originalString As String
    originalString = "123-ABC-456-XYZ"
    
    ' Loại bỏ dấu gạch nối
    Dim resultString As String
    resultString = Replace(originalString, "-", "")
    
    Debug.Print originalString ' Trước: 123-ABC-456-XYZ
    Debug.Print resultString ' Sau: 123ABC456XYZ
End Sub
```

### Sử dụng Biểu Thức Chính Quy

Đối với các mẫu phức tạp hơn, biểu thức chính quy cung cấp một lựa chọn mạnh mẽ.

Đầu tiên, kích hoạt thư viện Microsoft VBScript Regular Expressions thông qua Công cụ > Tham Chiếu trong Trình Biên Tập Visual Basic.

```basic
Sub DeletePatternChars()
    Dim regEx As Object
    Set regEx = CreateObject("VBScript.RegExp")
    
    Dim strPattern As String
    strPattern = "\d" ' Mẫu phù hợp với tất cả các chữ số
    
    With regEx
        .Global = True
        .IgnoreCase = True
        .Pattern = strPattern
    End With
    
    Dim originalString As String
    originalString = "Remove 123 and 456"
    
    ' Sử dụng phương thức Replace để xóa các kết quả phù hợp
    Dim resultString As String
    resultString = regEx.Replace(originalString, "")
    
    Debug.Print originalString ' Trước: Remove 123 and 456
    Debug.Print resultString ' Sau: Remove  và 
End Sub
```

## Sâu hơn

Lịch sử, việc khớp mẫu và thao tác chuỗi trong VBA đã bị hạn chế một phần, đặc biệt là khi so sánh với các ngôn ngữ lập trình hiện đại hơn cung cấp các thư viện tiêu chuẩn mở rộng cho những tác vụ này. Hàm `Replace` đơn giản và hiệu quả cho các thay thế trực tiếp nhưng thiếu linh hoạt cho việc khớp mẫu phức tạp hơn. Ở đây, biểu thức chính quy (RegEx) đem lại, cung cấp một cú pháp phong phú hơn cho việc khớp mẫu và thao tác chuỗi. Tuy nhiên, làm việc với RegEx trong VBA yêu cầu thiết lập thêm, chẳng hạn như kích hoạt tham chiếu Microsoft VBScript Regular Expressions, có thể là rào cản với những người dùng mới.

Mặc dù có những giới hạn này, sự giới thiệu hỗ trợ RegEx trong VBA là một bước tiến lớn, cung cấp một công cụ mạnh mẽ hơn cho các lập trình viên làm việc với xử lý văn bản. Trong các tình huống phức tạp hơn nơi các hàm chuỗi tích hợp không đủ, biểu thức chính quy cung cấp một lựa chọn linh hoạt và mạnh mẽ.

Đáng chú ý là, đối với những người làm việc trong môi trường hoặc dự án mà hiệu suất là quan trọng, việc tận dụng các thư viện bên ngoài hoặc tích hợp với các ngôn ngữ lập trình khác có thể cung cấp hiệu suất tốt hơn và nhiều tính năng hơn. Tuy nhiên, đối với nhiều nhiệm vụ hàng ngày trong VBA, những phương thức bản địa này vẫn là một lựa chọn thực tế và dễ tiếp cận.

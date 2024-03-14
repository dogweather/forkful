---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:23.470085-07:00
description: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u ch\u1EE7\
  \ y\u1EBFu \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng cho c\xE1c t\u1EC7p c\u1EA5u h\xEC\
  nh. C\xE1c l\u1EADp tr\xECnh vi\xEAn t\u1EADn\u2026"
lastmod: '2024-03-13T22:44:36.464093-06:00'
model: gpt-4-0125-preview
summary: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u ch\u1EE7\
  \ y\u1EBFu \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng cho c\xE1c t\u1EC7p c\u1EA5u h\xEC\
  nh. C\xE1c l\u1EADp tr\xECnh vi\xEAn t\u1EADn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
---

{{< edit_this_page >}}

## Gì và Tại sao?

TOML, viết tắt của Tom's Obvious, Minimal Language, là một định dạng tuần tự hóa dữ liệu chủ yếu được sử dụng cho các tệp cấu hình. Các lập trình viên tận dụng TOML vì tính dễ đọc và khả năng ánh xạ dễ dàng với các cấu trúc dữ liệu, cho phép cấu hình ứng dụng một cách đơn giản trên các môi trường lập trình khác nhau, bao gồm Visual Basic for Applications (VBA).

## Làm thế nào:

Làm việc với TOML trong VBA bao gồm việc phân tích tệp TOML để đọc cấu hình hoặc các thiết lập vào dự án VBA của bạn. VBA không có sẵn hỗ trợ cho TOML, vì vậy bạn thường sẽ sử dụng một trình phân tích hoặc chuyển đổi dữ liệu TOML sang một định dạng mà VBA có thể dễ dàng làm việc với, như JSON hoặc XML. Dưới đây là cách thủ công phân tích một tệp cấu hình TOML đơn giản:

1. **Tệp TOML Mẫu** (`config.toml`):
```
title = "Ví dụ TOML"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Mã VBA để Phân tích TOML**:

Giả sử nội dung TOML được đọc vào biến chuỗi `tomlStr`, đoạn mã VBA sau đây trình bày một cách đơn giản để phân tích phần `[database]`:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'Ví dụ truy cập dữ liệu đã phân tích
    Debug.Print "Máy chủ cơ sở dữ liệu: "; config("database")("server")
End Function
```

3. **Kết quả Mẫu** (Cửa sổ ngay lập tức):
```
Máy chủ cơ sở dữ liệu: 192.168.1.1
```

## Tìm hiểu Sâu

Sự chấp nhận thực tế của TOML trong cộng đồng phát triển phần mềm cho thấy xu hướng hướng tới các tệp cấu hình đơn giản, dễ đọc hơn, đặt trong sự tương phản với XML trước kia thịnh hành. Triết lý thiết kế của TOML nhấn mạnh về ngữ nghĩa rõ ràng và hướng đến việc phân tích đơn giản với chi phí tối thiểu. Trong VBA, xử lý trực tiếp TOML bao gồm việc phân tích thủ công hoặc sử dụng các công cụ bên ngoài để chuyển đổi TOML sang định dạng thân thiện hơn với VBA do thiếu hỗ trợ gốc. Mặc dù phương pháp phân tích thủ công này trình bày một cách tiếp cận cơ bản, việc sử dụng thư viện bên ngoài hoặc các định dạng trung gian như JSON có thể cung cấp các chiến lược phân tích mạnh mẽ và ít lỗi hơn. Với sự tích hợp rộng rãi của VBA với Microsoft Office, việc chuyển đổi TOML sang JSON và sử dụng khả năng phân tích JSON gốc của VBA (nơi áp dụng) hoặc các trình phân tích JSON của bên thứ ba có thể cung cấp một quy trình làm việc mượt mà hơn. Hơn nữa, với sự phát triển liên tục của các định dạng tuần tự hóa dữ liệu, lập trình viên cũng nên xem xét đến YAML, giống như TOML, nhấn mạnh về tính dễ đọc của con người nhưng đưa ra các sự đánh đổi khác biệt về mức độ phức tạp và linh hoạt.

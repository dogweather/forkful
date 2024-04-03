---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:04.308442-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ch\xECa kh\xF3a \u0111\u1EC3 g\u1EEDi m\u1ED9\
  t y\xEAu c\u1EA7u HTTP trong VBA l\xE0 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `Microsoft\
  \ XML, v6.0` (ho\u1EB7c c\xE1c phi\xEAn b\u1EA3n c\u0169 h\u01A1n, t\xF9y thu\u1ED9\
  c v\xE0o h\u1EC7 th\u1ED1ng c\u1EE7a\u2026"
lastmod: '2024-03-13T22:44:36.428587-06:00'
model: gpt-4-0125-preview
summary: "Ch\xECa kh\xF3a \u0111\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP trong\
  \ VBA l\xE0 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `Microsoft XML, v6.0` (ho\u1EB7\
  c c\xE1c phi\xEAn b\u1EA3n c\u0169 h\u01A1n, t\xF9y thu\u1ED9c v\xE0o h\u1EC7 th\u1ED1\
  ng c\u1EE7a b\u1EA1n)."
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Chìa khóa để gửi một yêu cầu HTTP trong VBA là sử dụng thư viện `Microsoft XML, v6.0` (hoặc các phiên bản cũ hơn, tùy thuộc vào hệ thống của bạn). Đầu tiên, đảm bảo tham chiếu này được kích hoạt trong dự án của bạn bằng cách đi tới Tools > References trong trình biên soạn VBA và kiểm tra `Microsoft XML, v6.0`.

Dưới đây là cách gửi một yêu cầu GET HTTP đơn giản:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Lỗi: " & .Status & " - " & .statusText
    End If
End With
```

Đối với yêu cầu POST, nơi chúng ta cần gửi dữ liệu (ví dụ: JSON) đến máy chủ:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Lỗi: " & .Status & " - " & .statusText
    End If
End With
```

Kết quả mẫu cho một yêu cầu thành công có thể là một chuỗi JSON hoặc một trang HTML, tùy thuộc vào API hoặc trang web mà bạn đang tương tác:

```
{"data": "Đây là phản hồi từ máy chủ"}
```

## Đào Sâu
Phương pháp được trình bày sử dụng đối tượng `MSXML2.XMLHTTP`, một phần của Microsoft XML Core Services (MSXML). Nó đã được giới thiệu để cung cấp cho các nhà phát triển VBA một cách thực hiện các hoạt động dựa trên XML và, theo thời gian, trở thành công cụ phổ biến cho các yêu cầu HTTP, ngay cả khi không làm việc trực tiếp với dữ liệu XML. Mặc dù đã lâu, nhưng nó vẫn là một lựa chọn đáng tin cậy cho các tương tác web đơn giản trong VBA.

Tuy nhiên, VBA và các cơ chế yêu cầu http của nó thiếu tự tin và linh hoạt so với môi trường lập trình hiện đại. Ví dụ, xử lý các yêu cầu bất đồng bộ hoặc làm việc trong các ứng dụng đòi hỏi các tính năng HTTP tiên tiến (như websockets hay sự kiện được gửi từ máy chủ) là ngoài phạm vi của VBA. Khi làm việc trên các dự án tích hợp web phức tạp hơn, các nhà phát triển thường sử dụng các thư viện hoặc công cụ bên ngoài, hoặc thậm chí tự động hóa hành vi trình duyệt qua các kỹ thuật web scraping, mặc dù những cách này là giải pháp tạm thời chứ không phải là giải pháp dài hạn.

Ngôn ngữ và môi trường như Python với thư viện `requests` của nó hoặc JavaScript chạy trên Node.js cung cấp khả năng yêu cầu HTTP mạnh mẽ và linh hoạt hơn ngay từ vỏ hộp, bao gồm các hoạt động bất đồng bộ, xử lý JSON dễ dàng, và hỗ trợ rộng rãi cho các công nghệ web khác nhau. Các nhà phát triển gắn bó với hệ sinh thái Microsoft có thể xem xét chuyển sang PowerShell hoặc C# cho các nhiệm vụ đòi hỏi tương tác web tinh vi hơn, tận dụng các tính năng lập trình mạng rộng lớn của .NET.

Như vậy, trong khi khả năng yêu cầu HTTP của VBA đủ tốt cho các nhiệm vụ truy vấn đơn giản và lấy dữ liệu, việc khám phá các lựa chọn thay thế trở nên quan trọng khi yêu cầu của dự án của bạn phát triển theo hướng phức tạp và hiện đại của cảnh quan web.

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:52.499057-07:00
description: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language\", l\xE0\
  \ m\u1ED9t ng\xF4n ng\u1EEF tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc cho con ng\u01B0\u1EDDi, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng cho c\xE1c t\u1EC7p c\u1EA5u h\xECnh. L\u1EADp\u2026"
lastmod: '2024-03-13T22:44:36.460116-06:00'
model: gpt-4-0125-preview
summary: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language\", l\xE0\
  \ m\u1ED9t ng\xF4n ng\u1EEF tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc cho con ng\u01B0\u1EDDi, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng cho c\xE1c t\u1EC7p c\u1EA5u h\xECnh. L\u1EADp\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Gì và Tại sao?

YAML, viết tắt của "YAML Ain't Markup Language", là một ngôn ngữ tuần tự hóa dữ liệu dễ đọc cho con người, thường được sử dụng cho các tệp cấu hình. Lập trình viên thường sử dụng nó vì sự đơn giản và dễ đọc của nó trong nhiều môi trường lập trình khác nhau, bao gồm cả trong lĩnh vực viết kịch bản của Visual Basic for Applications (VBA) nhằm tăng cường khả năng tương thích và lưu trữ, trao đổi dữ liệu.

## Làm thế nào:

Làm việc với YAML trong VBA đòi hỏi phải hiểu cách phân tích và chuyển đổi YAML thành định dạng mà VBA có thể dễ dàng thao tác, thường là dictionaries hoặc collections. Thật không may, VBA không hỗ trợ phân tích cú pháp hoặc tuần tự hóa YAML một cách tự nhiên. Tuy nhiên, bạn có thể sử dụng sự kết hợp của các công cụ chuyển đổi JSON và đối tượng dictionary để làm việc với dữ liệu YAML, xem xét mối quan hệ chặt chẽ của YAML với JSON.

Đầu tiên, chuyển đổi dữ liệu YAML của bạn sang JSON sử dụng một trình chuyển đổi trực tuyến hoặc công cụ chuyển đổi YAML thành JSON trong môi trường phát triển của bạn. Sau khi chuyển đổi, bạn có thể sử dụng ví dụ sau để phân tích JSON trong VBA, lưu ý rằng cách tiếp cận này gián tiếp cho phép bạn làm việc với YAML:

```vb
' Thêm tham chiếu đến Microsoft Scripting Runtime cho Dictionary
' Thêm tham chiếu đến Microsoft XML, v6.0 cho phân tích JSON

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Đây là JSON đã được chuyển từ YAML
    
    ' Giả sử bạn có một hàm phân tích JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Tên: " & parsedData("name")
    Debug.Print "Tuổi: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Chỗ giả định cho logic phân tích JSON - bạn có thể sử dụng thư viện bên ngoài ở đây
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
Trong ví dụ này, hàm `JsonParser` là nơi bạn sẽ phân tích JSON. Có các thư viện khác nhau hỗ trợ phân tích JSON, vì thư viện phân tích YAML trực tiếp cho VBA là khá ít.

## Sâu hơn nữa

Sự vắng mặt của việc xử lý trực tiếp YAML trong VBA có thể được giải thích bởi độ tuổi và môi trường mà nó được xây dựng cho, không được thiết kế ban đầu với các định dạng tuần tự hóa dữ liệu hiện đại. YAML xuất hiện như một định dạng cấu hình và tuần tự hóa phổ biến vào đầu những năm 2000, cùng với sự xuất hiện của các ứng dụng đòi hỏi tệp cấu hình thân thiện với con người hơn.

Lập trình viên thường sử dụng công cụ hoặc thư viện bên ngoài để cầu nối giữa VBA và YAML. Điều này thường liên quan đến việc chuyển đổi YAML sang JSON, như đã chỉ ra, do sự hỗ trợ JSON có sẵn qua các thư viện khác nhau và sự tương đồng giữa JSON và YAML về cấu trúc và mục đích.

Mặc dù làm việc trực tiếp với YAML trong VBA cho thấy sự linh hoạt của ngôn ngữ, đáng chú ý là các môi trường lập trình khác (ví dụ, Python hoặc JavaScript) cung cấp sự hỗ trợ nội bộ và liền mạch hơn cho YAML. Những lựa chọn này có thể phù hợp hơn cho các dự án phụ thuộc nặng vào YAML cho cấu hình hoặc tuần tự hóa dữ liệu. Dẫu vậy, đối với những ai cam kết hoặc yêu cầu VBA, phương pháp gián tiếp thông qua chuyển đổi JSON vẫn là một cách tiếp cận khả thi và hữu ích để quản lý và thao tác dữ liệu YAML.

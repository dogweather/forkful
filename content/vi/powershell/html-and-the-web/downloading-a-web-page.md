---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:39.804447-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 b\xF9\
  a ph\xE9p ma thu\u1EADt \u0111\u1EC3 t\u1EA3i m\u1ED9t trang web s\u1EED d\u1EE5\
  ng PowerShell. Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng `Invoke-WebRequest`."
lastmod: '2024-03-13T22:44:36.934643-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 b\xF9a ph\xE9p ma thu\u1EADt \u0111\u1EC3\
  \ t\u1EA3i m\u1ED9t trang web s\u1EED d\u1EE5ng PowerShell."
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
Dưới đây là bùa phép ma thuật để tải một trang web sử dụng PowerShell. Chúng ta sẽ sử dụng `Invoke-WebRequest`.

```PowerShell
# Lấy nội dung của example.com
$response = Invoke-WebRequest -Uri "http://example.com"

# Đây là những gì bạn nhận được
$response.Content
```

Kết quả mẫu:

```PowerShell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
    <!-- và tiếp tục như vậy -->
</head>
...
</html>
```

Bạn có thể chỉ muốn văn bản, không có thẻ HTML. Hãy làm điều đó:

```PowerShell
# Chỉ cần văn bản, làm ơn
$response.ParsedHtml.body.innerText
```

## Sâu hơn nữa
Ngày xưa, PowerShell không có cmdlet `Invoke-WebRequest` tuyệt vời. Các lập trình viên sẽ sử dụng lớp .NET `System.Net.WebClient` hoặc phải nhờ đến các công cụ bên ngoài. Bây giờ, tất cả đã được tích hợp sẵn, làm cho công việc của chúng ta đơn giản hơn.

`Invoke-WebRequest` cung cấp nhiều hơn là chỉ nội dung. Tiêu đề, trạng thái, và thông tin phiên – tất cả đều có mặt. Nếu bạn đang vọc vạch với API, bạn sẽ yêu thích `Invoke-RestMethod` như một lựa chọn tập trung.

Bên dưới lớp vỏ, các cmdlet này dựa vào lớp .NET HttpClient cồng kềnh, chứa đựng độ tin cậy và chức năng rộng rãi.

Và, nếu bạn đang không kiên nhẫn chờ đợi trang web tải xuống, `Invoke-WebRequest` cũng hỗ trợ các thao tác bất đồng bộ. Tuy nhiên, đó là chủ đề cho một ngày khác.

## Xem thêm
- Tài liệu [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- Thêm thông tin về [Invoke-RestMethod cho tương tác API](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Một [kho GitHub PowerShell](https://github.com/PowerShell/PowerShell) dành cho các lập trình viên tò mò muốn nhìn vào bên trong.

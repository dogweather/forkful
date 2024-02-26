---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:28.600677-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n trong Visual Basic for Applications (VBA) l\xE0 truy c\u1EADp v\xE0o\
  \ c\xE1c ngu\u1ED3n web \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7 b\u1EDFi t\xEAn ng\u01B0\
  \u1EDDi d\xF9ng v\xE0 m\u1EADt\u2026"
lastmod: '2024-02-25T18:49:34.768983-07:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n trong Visual Basic for Applications (VBA) l\xE0 truy c\u1EADp v\xE0o\
  \ c\xE1c ngu\u1ED3n web \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7 b\u1EDFi t\xEAn ng\u01B0\
  \u1EDDi d\xF9ng v\xE0 m\u1EADt\u2026"
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản trong Visual Basic for Applications (VBA) là truy cập vào các nguồn web được bảo vệ bởi tên người dùng và mật khẩu. Các lập trình viên thực hiện điều này để tương tác với các API hoặc dịch vụ web bảo mật trong ứng dụng VBA của họ, chẳng hạn như tự động hóa các tác vụ trong Excel hoặc Access với dữ liệu từ các điểm cuối được bảo mật.

## Cách thực hiện:

Trong VBA, bạn có thể sử dụng thư viện `Microsoft XML, v6.0` (MSXML2) để gửi các yêu cầu HTTP với xác thực cơ bản. Điều này liên quan đến việc thiết lập tiêu đề `"Authorization"` của yêu cầu để bao gồm thông tin đăng nhập ở dạng mã hóa base64. Dưới đây là hướng dẫn từng bước:

1. **Tham chiếu MSXML2**: Đầu tiên, đảm bảo dự án VBA của bạn tham chiếu thư viện `Microsoft XML, v6.0`. Trong trình chỉnh sửa VBA, đi đến Công cụ > Tham chiếu và kiểm tra `Microsoft XML, v6.0`.

2. **Tạo và gửi yêu cầu HTTP**: Sử dụng đoạn mã VBA sau đây làm hướng dẫn. Thay thế `"your_username"` và `"your_password"` bằng thông tin đăng nhập thực tế của bạn và điều chỉnh URL theo nhu cầu.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Thay thế bằng URL thực tế
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Xuất kết quả phản hồi ra cửa sổ Immediate
    ```

3. **Mã hóa thông tin đăng nhập ở dạng base64**: VBA không có hàm tích hợp sẵn cho việc mã hóa base64, nhưng bạn có thể sử dụng hàm `EncodeBase64` tùy chỉnh này:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Điều này sẽ gửi một yêu cầu GET đến `http://example.com/api/resource` với thông tin xác thực cơ bản đã chỉ định và in phản ứi ra.

## Sâu hơn

Phương pháp sử dụng ở đây, mặc dù hiệu quả cho các trường hợp sử dụng đơn giản, dựa vào lược đồ xác thực cơ bản, gửi thông tin đăng nhập trong một định dạng dễ giải mã (mã hóa base64 không phải là mã hóa). Do tính dễ bị tổn thương, đặc biệt là trong các ngữ cảnh không phải HTTPS, xác thực cơ bản không được khuyến nghị để truyền dữ liệu nhạy cảm qua internet mà không có các lớp bảo mật bổ sung như SSL/TLS.

Lịch sử, xác thực cơ bản là một trong những phương pháp đầu tiên được phát triển để kiểm soát quyền truy cập vào các nguồn web. Ngày nay, các tiêu chuẩn xác thực an toàn và linh hoạt hơn, chẳng hạn như OAuth 2.0, thường được ưu tiên cho các ứng dụng mới. Do giới hạn của VBA và các phụ thuộc bên ngoài cần thiết cho các phương pháp xác thực tiên tiến hơn, các lập trình viên thường sử dụng VBA trong môi trường nội bộ hoặc ít quan trọng về mặt bảo mật, hoặc sử dụng nó làm bước đệm để nhanh chóng mô phỏng ý tưởng.

Khi sử dụng VBA cho các yêu cầu HTTP, hãy nhớ rằng mỗi phiên bản của thư viện MSXML có thể hỗ trợ các tính năng và tiêu chuẩn an ninh khác nhau. Hãy luôn sử dụng phiên bản mới nhất tương thích với ứng dụng của bạn để đảm bảo an toàn và hiệu suất tốt hơn. Ngoài ra, hãy xem xét các hạn chế về môi trường và các tính năng có khả năng bị loại bỏ khi chọn VBA cho các dự án mới, đặc biệt là những dự án đòi hỏi giao tiếp HTTP an toàn. Các môi trường lập trình hoặc ngôn ngữ khác có thể cung cấp các giải pháp mạnh mẽ, an toàn và bảo trì hơn cho các tác vụ tương tự.

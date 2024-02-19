---
aliases:
- /vi/vba/parsing-html/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:31.632145-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Visual Basic for Applications\
  \ (VBA) bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t th\xF4ng tin c\u1EE5 th\u1EC3\
  \ t\u1EEB m\u1ED9t t\xE0i li\u1EC7u HTML. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m\u2026"
lastmod: 2024-02-18 23:08:50.506684
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Visual Basic for Applications (VBA)\
  \ bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t th\xF4ng tin c\u1EE5 th\u1EC3 t\u1EEB\
  \ m\u1ED9t t\xE0i li\u1EC7u HTML. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Phân tích cú pháp HTML trong Visual Basic for Applications (VBA) bao gồm việc trích xuất thông tin cụ thể từ một tài liệu HTML. Các lập trình viên làm điều này để tự động hóa quá trình đọc và xử lý dữ liệu từ các trang web, như kho scrape nội dung website hay tự động hóa việc gửi biểu mẫu và truy xuất dữ liệu, trong các ứng dụng như Microsoft Excel hay Access hỗ trợ VBA.

## Cách thực hiện:

Trong VBA, bạn có thể phân tích cú pháp HTML sử dụng `Thư viện Đối tượng HTML của Microsoft`. Thêm một tham khảo đến thư viện này trong trình biên tập VBA của bạn bằng cách đi tới Công cụ > Tham khảo và kiểm tra `Thư viện Đối tượng HTML của Microsoft`. Điều này cho phép bạn truy cập vào các lớp để điều hướng và thao tác với tài liệu HTML.

Dưới đây là một ví dụ đơn giản cho thấy cách tải một tài liệu HTML từ một tệp và trích xuất tất cả các liên kết (thẻ neo):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Tải nội dung HTML từ một tệp
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Khởi tạo Tài liệu HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Lấy tất cả các thẻ neo
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Lặp qua tất cả các phần tử neo và in thuộc tính href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Đoạn mã này đọc nội dung tệp HTML, tải nó vào một đối tượng `HTMLDocument`, truy xuất tất cả các phần tử neo (`<a>` tags), rồi sau đó lặp qua chúng, in thuộc tính `href` của mỗi thẻ ra Cửa sổ Ngay lập tức.

## Phân tích sâu hơn:

Trong lịch sử, việc phân tích cú pháp HTML trong VBA đã có phần cấp dưỡng do thiếu hỗ trợ trực tiếp cho các công nghệ scrape web và xử lý tài liệu hiện đại. Thư viện Đối tượng HTML của Microsoft, mặc dù mạnh mẽ, nhưng hơi lỗi thời và có thể không xử lý mượt mà các tiêu chuẩn web hiện đại như các công nghệ mới hơn.

Đối với các nhiệm vụ phân tích cú pháp HTML và scrape web phức tạp, các công cụ và ngôn ngữ thay thế như Python với các thư viện như Beautiful Soup hay Scrapy thường được khuyến nghị. Những công cụ hiện đại này cung cấp nhiều linh hoạt hơn, hiệu suất tốt hơn và được cập nhật theo các tiêu chuẩn web hiện tại. Tuy nhiên, khi làm việc trong hệ sinh thái Microsoft Office, việc sử dụng VBA với Thư viện Đối tượng HTML của Microsoft vẫn là một kỹ năng quý giá. Nó mở khóa khả năng thao tác trực tiếp với nội dung HTML một cách mượt mà với các ứng dụng như Excel và Access, cung cấp một phương pháp trực tiếp để hoàn thành các nhiệm vụ liên quan đến xử lý tài liệu HTML cơ bản mà không cần phải rời khỏi môi trường VBA quen thuộc.

---
title:                "Phân Tích Cú Pháp HTML"
aliases:
- /vi/swift/parsing-html.md
date:                  2024-01-28T22:04:26.065031-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Phân tích cú pháp HTML có nghĩa là lọc qua "bát canh" mã của một trang web để tìm những thông tin hữu ích — văn bản, liên kết, hình ảnh, v.v. Các lập trình viên làm điều này để trích xuất dữ liệu, tự động hóa tương tác web, hoặc nhập nội dung vào ứng dụng của họ.

## Làm thế nào:

Swift không có chức năng phân tích cú pháp HTML sẵn có; chúng ta cần một công cụ hỗ trợ. Hãy sử dụng SwiftSoup, một thư viện Swift giống như BeautifulSoup của Python. Đầu tiên, thêm SwiftSoup vào dự án của bạn sử dụng Swift Package Manager.

Dưới đây là cách thực hiện:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Lần phân tích đầu tiên</title></head>"
                + "<body><p>Đã phân tích cú pháp HTML thành tài liệu.</p></body></html>"
    let doc = try SwiftSoup.parse(html)
    let title = try doc.title()
    let bodyText = try doc.body()?.text()
    
    print(title) // Đầu ra: Lần phân tích đầu tiên
    print(bodyText) // Đầu ra: Đã phân tích cú pháp HTML thành tài liệu.
} catch Exception.Error(let type, let message) {
    print("Lỗi loại: \(type) xảy ra: \(message)")
} catch {
    print("Đã xảy ra lỗi không xác định")
}
```

## Sâu hơn

HTML, hay Ngôn ngữ Đánh dấu Siêu Văn bản, đã là xương sống của web từ khi Tim Berners-Lee giới thiệu nó (và web) vào năm 1991. Khi web phát triển, HTML cũng vậy, làm tăng độ phức tạp của việc phân tích cú pháp.

Dưới đây là lý do SwiftSoup tỏa sáng:
- **Thân thiện với Người dùng**: API của nó phản chiếu JQuery, nghĩa là nó trực quan cho những ai quen với phát triển web.
- **Robustness**: Xử lý tốt các lỗi thực tế của HTML.
- **Hiệu suất**: Swift nhanh, điều này quan trọng đối với công việc phân tích cú pháp lớn.

Có phương án khác? Chắc chắn rồi!
- **WebKit**: Sử dụng cho các nhiệm vụ nặng như render trang web hoặc thực thi JavaScript.
- **libxml2**: Lối đi C cứng cáp, nhưng bạn phải sẵn sàng cho thách thức.
- **Regex**: Đơn giản là không. Đó không phải là một công cụ phân tích cú pháp. Đừng cố gắng “phân tích cú pháp” HTML với regex. Nghiêm túc mà nói.

Tuy nhiên, hãy nhớ rằng một công cụ phân tích cú pháp giống như SwiftSoup không chỉ đọc trang như nó vốn có; nó không biết đến bất kỳ nội dung nào được tải động bằng JavaScript. Đối với điều đó, hãy hướng tới các giải pháp liên quan đến WebKit hoặc chế độ không giao diện của trình duyệt.

## Xem Thêm

- SwiftSoup trên GitHub: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift Package Manager: [https://swift.org/package-manager/](https://swift.org/package-manager/)
- Tài liệu WebKit: [https://developer.apple.com/documentation/webkit](https://developer.apple.com/documentation/webkit)
- Xử lý nội dung động: [Selenium WebDriver](https://www.selenium.dev/documentation/en/) (không cụ thể cho Swift nhưng có liên quan cho tương tác tự động với trang web động)

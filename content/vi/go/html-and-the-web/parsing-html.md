---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:43.745544-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Go bao g\u1ED3m vi\u1EC7c ph\xE2\
  n t\xEDch n\u1ED9i dung c\u1EE7a c\xE1c t\u1EADp tin HTML \u0111\u1EC3 tr\xEDch\
  \ xu\u1EA5t d\u1EEF li\u1EC7u, thao t\xE1c c\u1EA5u tr\xFAc, ho\u1EB7c chuy\u1EC3\
  n \u0111\u1ED5i HTML sang\u2026"
lastmod: '2024-03-13T22:44:35.979043-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Go bao g\u1ED3m vi\u1EC7c ph\xE2\
  n t\xEDch n\u1ED9i dung c\u1EE7a c\xE1c t\u1EADp tin HTML \u0111\u1EC3 tr\xEDch\
  \ xu\u1EA5t d\u1EEF li\u1EC7u, thao t\xE1c c\u1EA5u tr\xFAc, ho\u1EB7c chuy\u1EC3\
  n \u0111\u1ED5i HTML sang c\xE1c \u0111\u1ECBnh d\u1EA1ng kh\xE1c."
title: "Ph\xE2n T\xEDch HTML"
weight: 43
---

## Làm thế nào:
Để phân tích cú pháp HTML trong Go, bạn thường sử dụng gói `goquery` hoặc gói `net/html` của thư viện chuẩn. Dưới đây là một ví dụ cơ bản sử dụng `net/html` để trích xuất tất cả các liên kết từ một trang web:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Lấy tài liệu HTML
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Phân tích cú pháp tài liệu HTML
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Hàm để duyệt qua DOM một cách đệ quy
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Duyệt qua DOM
    f(doc)
}
```

Kết quả mẫu (giả sử `http://example.com` chứa hai liên kết):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Mã này yêu cầu một trang HTML, phân tích cú pháp nó, và duyệt qua DOM một cách đệ quy để tìm và in các thuộc tính `href` của tất cả các thẻ `<a>`.

## Tìm hiểu sâu
Gói `net/html` cung cấp các cơ bản để phân tích cú pháp HTML trong Go, trực tiếp thực thi các thuật toán token hóa và xây dựng cây được quy định bởi tiêu chuẩn HTML5. Cách tiếp cận cấp thấp này mạnh mẽ nhưng có thể rườm rà cho các nhiệm vụ phức tạp.

Ngược lại, gói `goquery` của bên thứ ba, được lấy cảm hứng từ jQuery, cung cấp một giao diện cấp cao hơn đơn giản hóa việc thao tác và duyệt qua DOM. Nó cho phép các nhà phát triển viết mã ngắn gọn và biểu cảm cho các nhiệm vụ như chọn phần tử, trích xuất thuộc tính, và thao tác nội dung.

Tuy nhiên, sự tiện lợi của `goquery` đi kèm với chi phí là một phụ thuộc bổ sung và hiệu suất tiềm năng chậm hơn do lớp trừu tượng của nó. Sự lựa chọn giữa `net/html` và `goquery` (hoặc các thư viện phân tích cú pháp khác) phụ thuộc vào các yêu cầu cụ thể của dự án, chẳng hạn như nhu cầu tối ưu hóa hiệu suất hoặc dễ sử dụng.

Về lịch sử, việc phân tích cú pháp HTML trong Go đã phát triển từ các hoạt động trên chuỗi cơ bản đến việc thao tác cây DOM tinh vi, phản ánh sự phát triển của hệ sinh thái ngôn ngữ và nhu cầu của cộng đồng về các công cụ khai thác web và trích xuất dữ liệu mạnh mẽ. Mặc dù có khả năng tự phát, sự phổ biến của các thư viện bên thứ ba như `goquery` nổi bật sở thích của cộng đồng Go về mã nguồn mở, tái sử dụng. Tuy nhiên, đối với các ứng dụng quan trọng về hiệu suất, lập trình viên có thể vẫn ưu tiên gói `net/html` hoặc thậm chí sử dụng regex cho các nhiệm vụ phân tích cú pháp đơn giản, giữ trong tâm trí những rủi ro và hạn chế của việc phân tích cú pháp HTML dựa trên regex.

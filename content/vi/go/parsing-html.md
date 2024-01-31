---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:03:57.327676-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Phân tích cú pháp HTML có nghĩa là trích xuất thông tin từ một tệp HTML - đó là mã đằng sau các trang web. Lập trình viên làm điều đó để tự động hóa việc truy xuất dữ liệu, trích xuất nội dung và chuyển giao nội dung giữa các hệ thống.

## Làm thế nào:
Go có gói `net/html` hoàn hảo cho việc lặn sâu vào HTML. Dưới đây là bản chất của nó:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"os"
)

func main() {
	// Tải HTML
	resp, err := http.Get("http://example.com")
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetch: %v\n", err)
		os.Exit(1)
	}
	defer resp.Body.Close()
	
	// Phân tích cú pháp HTML
	doc, err := html.Parse(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parse: %v\n", err)
		os.Exit(1)
	}

	// Duyệt qua cây nút HTML
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Printf("%v\n", a.Val)
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```

Chạy nó? Bạn sẽ nhận được các liên kết từ `example.com`, in ra console của bạn. Dễ dàng!

## Sâu hơn nữa:
Dưới đây là thông tin chi tiết. Khi web mới ra đời, HTML rất đơn giản. Nhưng giờ đây, không còn nữa. Ngày nay, nó phức tạp, chứa đầy các sắc thái.

Tại sao không dùng regex? HTML có thể không nhất quán. Regex cho HTML là một cách tiếp cận dễ lỗi và không ổn định. Các parser như `net/html` thông minh hơn. Chúng xử lý các điều kỳ lạ và sự lồng nhau trong HTML mà sẽ làm hỏng một mẫu regex.

Bộ phân tích cú pháp `net/html` xây dựng một cây từ các phần tử HTML. Nó giống như cấu trúc hóa một đống cành lộn xộn - biến hỗn loạn thành thứ gì đó bạn có thể trèo. Bạn duyệt qua cây với các hàm của mình để lọc qua các thẻ và thuộc tính.

Bạn có thể sử dụng cái gì khác? Các thư viện như `goquery` mang lại trải nghiệm giống như jQuery cho Go, và `colly` là một lựa chọn phổ biến cho việc scraping.

## Xem thêm:
- Gói `net/html` của Go: https://pkg.go.dev/golang.org/x/net/html
- GoQuery cho cú pháp giống như jQuery: https://github.com/PuerkitoBio/goquery
- Colly cho việc scraping: http://go-colly.org/

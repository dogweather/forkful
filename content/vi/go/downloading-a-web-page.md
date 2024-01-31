---
title:                "Tải trang web"
date:                  2024-01-28T21:59:22.912984-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web xuống có nghĩa là lấy nội dung của nó thông qua HTTP. Lập trình viên thực hiện việc này để tương tác với các dịch vụ web, thu thập dữ liệu hoặc theo dõi thời gian hoạt động của trang web.

## Cách thực hiện:

Trong Go, việc tải một trang web xuống là một việc rất đơn giản với gói `net/http`. Dưới đây là tóm tắt nhanh:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```

Chạy nó, và bạn sẽ nhận được HTML của `http://example.com` hiện lên màn hình của bạn, có thể kèm theo một số tiêu đề HTTP.

## Đi sâu hơn

Trước đây, việc lấy nội dung web là một miền hoang dã của lập trình socket và yêu cầu HTTP tự tạo. Giờ đây, các thư viện như `http` của Go đã lấy đi công việc ngặt nghèo khỏi tay chúng ta.

Tại sao không chỉ dùng `curl` hoặc `wget`? Tự động hóa, bạn của tôi. Nhúng logic tải xuống vào mã code của bạn làm cho nó có thể lặp lại và tích hợp được.

Bên dưới, `http.Get` thực hiện một yêu cầu GET, quản lý cookie, và nhiều hơn nữa. Bạn có thể kiểm soát thời gian chờ, tiêu đề, và đi sâu vào các phương thức vận chuyển tùy chỉnh. Nhưng đó là một câu chuyện cho một ngày khác.

Đối với các lựa chọn thay thế, bạn có thể cân nhắc sử dụng `http.Client` nếu bạn cần nhiều kiểm soát hơn, hoặc các gói từ bên thứ ba như `gorequest` cho một hương vị khác.

## Xem thêm

- Tài liệu gói net/http của Go: https://pkg.go.dev/net/http
- Effective Go để hiểu rõ các phương pháp tốt nhất: https://golang.org/doc/effective_go
- Go by Example để có thêm các đoạn mã thực hành: https://gobyexample.com

---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:57.879547-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Gửi một yêu cầu HTTP là cách chương trình của bạn yêu cầu một hệ thống khác cung cấp dữ liệu hoặc gửi dữ liệu cho nó. Lập trình viên thực hiện điều này để tương tác với các dịch vụ web, API, và để trao đổi thông tin qua internet.

## Cách thực hiện:
Dưới đây là đoạn mã trong Go để gửi một yêu cầu GET và xử lý phản hồi:

```Go
package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	response, err := http.Get("https://api.example.com/data")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	if response.StatusCode == http.StatusOK {
		body, readErr := io.ReadAll(response.Body)
		if readErr != nil {
			log.Fatal(readErr)
		}
		os.Stdout.Write(body)
	} else {
		log.Printf("Nhận trạng thái phản hồi không OK: %s", response.Status)
	}
}
```

Dưới đây là những gì bạn có thể thấy sau khi chạy đoạn mã này:

```
{"name":"John Doe","occupation":"Software Developer"}
```

## Sâu hơn

Trước khi gói `net/http` của Go làm cho mọi thứ dễ dàng hơn, việc gửi yêu cầu HTTP là một nỗi đau. Những ngày đầu, chúng ta phải xử lý lập trình socket cấp thấp, đó là cả một quá trình quản lý kết nối TCP và các giao thức một cách thủ công. Ngày nay, thư viện chuẩn đã tóm lược những phức tạp này.

Mặc dù `http.Get` tiện lợi cho các yêu cầu đơn giản, khi bạn cần nhiều kiểm soát hơn, `http.NewRequest` và `http.Client` là các người bạn của bạn. Chúng cho phép bạn chỉnh sửa tiêu đề, thiết lập thời gian chờ và xử lý chuyển hướng một cách chính xác hơn.

Điểm để suy ngẫm: `http.Get` và các bạn của nó là các lời gọi chặn. Chúng không trả về cho đến khi nhận được phản hồi HTTP hoàn chỉnh. Trong một ứng dụng có lưu lượng truy cập cao, sử dụng các tính năng đồng thời của Go như goroutines và channels để tránh làm chậm.

Các lựa chọn khác bao gồm các gói của bên thứ ba như `Resty` hoặc `GoReq`. Một số ưa chuộng chúng vì giao diện trực quan và chức năng bổ sung. Luôn cân nhắc xem lợi ích có lớn hơn chi phí thêm một sự phụ thuộc hay không.

## Xem thêm

- Tài liệu gói Go net/http: [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- Effective Go – Concurrency: [https://golang.org/doc/effective_go#concurrency](https://golang.org/doc/effective_go#concurrency)
- Go by Example – HTTP Clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
- Sách "Ngôn ngữ lập trình Go" để hiểu sâu hơn về thư viện chuẩn của Go.

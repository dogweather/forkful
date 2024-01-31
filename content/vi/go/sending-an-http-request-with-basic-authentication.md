---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:11.073219-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Yêu cầu HTTP với xác thực cơ bản thêm một lớp bảo mật đơn giản vào một cuộc gọi API. Lập trình viên sử dụng nó để truy cập vào tài nguyên cần có thông tin xác thực, như dữ liệu cụ thể của người dùng.

## Cách thực hiện:
Gửi một yêu cầu HTTP đã được xác thực là điều dễ dàng trong Go:

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "https://api.example.com/data", nil)
	if err != nil {
		panic(err)
	}

	username := "user"
	password := "pass"
	credentials := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic "+credentials)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%s\n", body)
}
```

Kết quả mẫu (với URL API và thông tin xác thực giả tưởng):
```plaintext
{"status":"success","data":"some private data"}
```

## Sâu hơn
Xác thực cơ bản là một phần của quy chuẩn HTTP/1.0 và đã tồn tại từ những ngày đầu của web. Mặc dù không phải là bảo mật nhất (thông tin xác thực chỉ được mã hóa base64, không được mã hóa), vì vậy, nó thường được thay thế bằng OAuth hoặc JWT trong các ứng dụng nhạy cảm hơn.

Về mặt triển khai, Go bao gồm hỗ trợ sẵn có cho khách hàng và yêu cầu HTTP, với gói `net/http` giúp các nhà phát triển xử lý giao thông web. Khi sử dụng xác thực cơ bản, chúng ta cần đảm bảo thông tin xác thực được mã hóa một cách thích hợp và tiêu đề `Authorization` được thêm vào yêu cầu HTTP.

Mặc dù đơn giản, bạn nên tránh sử dụng xác thực cơ bản qua HTTP thông thường do nó dễ bị tấn công man-in-the-middle. Luôn sử dụng HTTPS khi bạn gửi thông tin xác thực.

## Tham khảo thêm
- Tài liệu gói Go `net/http`: https://pkg.go.dev/net/http
- Tài liệu gói Go `encoding/base64`: https://pkg.go.dev/encoding/base64
- Thông tin về Xác thực Cơ bản HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Đối với các phương pháp xác thực an toàn hơn: https://oauth.net/ và https://jwt.io/

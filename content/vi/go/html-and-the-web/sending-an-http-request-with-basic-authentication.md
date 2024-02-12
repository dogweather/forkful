---
title:                "Gửi yêu cầu HTTP với xác thực cơ bản"
aliases:
- /vi/go/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-03T18:09:43.189384-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi yêu cầu HTTP với xác thực cơ bản"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc gửi một yêu cầu HTTP với xác thực cơ bản trong Go bao gồm việc thêm một tiêu đề ủy quyền vào yêu cầu của bạn bao gồm tên người dùng và mật khẩu dưới dạng một chuỗi được mã hóa Base64. Các lập trình viên sử dụng phương pháp này để truy cập vào các nguồn tài nguyên đòi hỏi xác minh người dùng, đảm bảo rằng ứng dụng của họ có thể tương tác một cách an toàn với các dịch vụ qua web.

## Cách thực hiện:

Để thực hiện một yêu cầu HTTP với xác thực cơ bản trong Go, bạn cần chế tạo các tiêu đề yêu cầu của mình để bao gồm trường `Authorization`, được điền bằng thông tin đăng nhập của bạn ở định dạng đúng. Dưới đây là một ví dụ minh họa cách thực hiện một yêu cầu GET đến một điểm cuối API đòi hỏi xác thực cơ bản:

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // Mã hóa thông tin đăng nhập
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Thiết lập tiêu đề Authorization
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Trạng thái phản hồi:", resp.Status)
}
```

Khi chạy đoạn mã này sẽ gửi một yêu cầu GET đến URL đã chỉ định với tiêu đề Authorization cần thiết. Đầu ra sẽ trông như thế này, tùy thuộc vào điểm cuối và dịch vụ của bạn:

```
Trạng thái phản hồi: 200 OK
```

## Sâu thêm

Xác thực cơ bản trong các yêu cầu HTTP là một phương pháp được hỗ trợ rộng rãi để thực thi các kiểm soát truy cập đối với các nguồn tài nguyên web. Nó đơn giản là gửi một tên người dùng và mật khẩu với mỗi yêu cầu, làm cho nó dễ dàng thực hiện nhưng không phải là phương pháp an toàn nhất có sẵn. Một nhược điểm lớn là, trừ khi được sử dụng kết hợp với SSL/TLS, các thông tin đăng nhập được gửi dưới dạng văn bản rõ ràng (vì Base64 dễ dàng được giải mã). Điều này có thể tiềm ẩn lộ thông tin nhạy cảm cho các cuộc tấn công man-in-the-middle.

Trong Go, việc gửi những yêu cầu này bao gồm việc thao tác trực tiếp với tiêu đề `Authorization`. Mặc dù thư viện tiêu chuẩn của Go (`net/http`) cung cấp các nguyên tắc mạnh mẽ để xử lý giao tiếp HTTP(s), nó tương đối ở mức độ thấp, yêu cầu các nhà phát triển tự mình xử lý các khía cạnh khác nhau của việc xử lý yêu cầu/phản hồi HTTP. Điều này mang lại nhiều tính linh hoạt cho các lập trình viên nhưng cũng có nghĩa là họ cần phải chú ý nhiều hơn đến các hàm ý an ninh, mã hóa và quản lý tiêu đề một cách chính xác.

Đối với các ứng dụng đòi hỏi an ninh cao hơn, nên cân nhắc sử dụng các hệ thống xác thực tiên tiến hơn như OAuth2 hay JWT (JSON Web Tokens). Những phương pháp này cung cấp các tính năng bảo mật mạnh mẽ hơn và được hỗ trợ rộng rãi trên các API và dịch vụ hiện đại. Hệ sinh thái đang mở rộng của Go bao gồm nhiều thư viện và công cụ (như `golang.org/x/oauth2`, trong số khác) để tạo điều kiện cho những phương pháp xác thực an toàn hơn, giúp các nhà phát triển triển khai các cơ chế ủy quyền hiệu quả, an toàn và hiện đại trong các ứng dụng của họ.

---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:07.987784-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản có nghĩa là bạn đang đi kèm một tên người dùng và mật khẩu với yêu cầu của mình, như một cử chỉ bí mật để truy cập vào một nguồn web. Các lập trình viên làm điều này để đảm bảo rằng chỉ những người có thông tin (tức là, với thông tin xác thực đúng) mới có thể truy cập.

## Làm thế nào:

Trong Gleam, bạn sẽ sử dụng thư viện `gleam/http`. Đây là một ví dụ đơn giản:

```gleam
import gleam/http.{Request, BasicAuth, get}

pub fn fetch_resource_with_basic_auth() {
  let auth = BasicAuth(
    username: "awesome_dev",
    password: "superSecret123",
  )
  let request = Request(
    method: Get,
    url: "https://api.example.com/protected",
    headers: [],
    body: None,
    basic_auth: Some(auth),
  )

  assert Ok(response) = get(request)
  response
}
```

Những gì bạn sẽ thấy khi chạy nó:

```gleam
Ok(#{
  status: 200,
  headers: [...],
  body: "Đây là dữ liệu bí mật của bạn!"
})
```

Hoặc, nếu thông tin xác thực sai:

```gleam
Ok(#{
  status: 401,
  headers: [...],
  body: ""
})
```

## Tìm hiểu sâu hơn

Ngày xưa, xác thực cơ bản là một trong những phương pháp đầu tiên để bảo mật giao tiếp web; nó giống như một ổ khóa cổ điển—đơn giản nhưng không phải là an toàn nhất.

Các phương pháp khác? Bạn có OAuth cho các tình huống phức tạp hơn, hoặc xác thực dựa trên token cho một sự thỏa hiệp giữa sự đơn giản và bảo mật.

Về mặt triển khai, xác thực cơ bản trong HTTP đưa một chuỗi được mã hóa Base64 (kết hợp tên người dùng và mật khẩu của bạn) vào một tiêu đề 'Authorization'. Nó không được mã hóa, do đó lý do tại sao nó được coi là cơ bản và không được khuyến nghị cho những việc nhạy cảm mà không có HTTPS ít nhất. Ngoài ra, đừng mã hóa cứng thông tin xác thực trong mã của bạn; sử dụng biến môi trường hoặc một dịch vụ lưu trữ an toàn.

## Xem thêm

Tìm hiểu sâu hơn vào:

- [Xác thực HTTP trên MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Lưu trữ an toàn cho bí mật ứng dụng](https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets)

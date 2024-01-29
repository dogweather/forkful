---
title:                "Tải trang web"
date:                  2024-01-28T21:59:15.842354-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải xuống một trang web có nghĩa là lấy nội dung của nó qua HTTP. Các lập trình viên thực hiện điều này cho việc web scraping, phân tích dữ liệu, hoặc để tương tác với các dịch vụ web.

## Cách thực hiện:

Hãy lấy một trang web sử dụng Gleam với gói `gleam_http`. Giả sử `gleam_http` và `gleam_otp` đã có trong các phụ thuộc dự án của bạn.

```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() -> Result(String, Nil) {
  let response = httpc.send(http.Request(to: "http://example.com")) // Thực hiện yêu cầu GET
  should.equal(response.status, 200) // Xác nhận mã trạng thái là OK
  Ok(response.body) // Trả lại phần thân của phản hồi
}

```

Kết quả mẫu sau khi chạy mã của bạn có thể trông như thế này:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Sâu xa hơn

Ngày xưa, vào những ngày đầu của web, tải xuống một trang web đơn giản như telnet đến cổng 80. Ngày nay, bạn có các thư viện và ngôn ngữ, như Gleam, chăm sóc các chi tiết HTTP khó khăn.

Các lựa chọn thay thế cho `gleam_http` bao gồm các thư viện cấp thấp hơn hoặc giao tiếp với các thư viện Erlang/Elixir khác sử dụng các tính năng tương tác của Gleam.

Hàm `gleam_http` là `httpc.send()` đang làm phần nặng nhọc trong ví dụ của chúng tôi. Nó được xây dựng trên mô-đun Erlang `httpc`, cung cấp một API trực quan với một chút an toàn kiểu và khớp mẫu của Gleam.

## Xem Thêm

- Tài liệu Gleam: https://hexdocs.pm/gleam/gleam_http/
- Kho GitHub `gleam_http`: https://github.com/gleam-lang/http
- Một bài giới thiệu về HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Để có cái nhìn sâu sắc về web scraping, hãy kiểm tra Beautiful Soup cho Python: https://www.crummy.com/software/BeautifulSoup/

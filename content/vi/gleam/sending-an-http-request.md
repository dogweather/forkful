---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:51.632856-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Gửi một yêu cầu HTTP là cách chương trình của bạn yêu cầu dữ liệu từ nơi khác trên internet. Lập trình viên làm điều này để tương tác với các dịch vụ web, lấy dữ liệu mới, hoặc để tích hợp các API bên ngoài vào ứng dụng của họ.

## Cách thực hiện:
Trong Gleam, chúng ta chưa có một HTTP client được tích hợp sẵn, vì vậy hãy sử dụng thư viện `gleam_http`. Dưới đây là một GET request đơn giản:

```gleam
import gleam/http.{Response, Error}
import gleam/httpc

pub fn main() -> Result(Response, Error) {
  // Gửi một GET request đến example.com
  httpc.send(httpc.Request(
    method: httpc.Get,
    url: "http://example.com",
    headers: [],
    body: httpc.Empty,
  ))
}
```

Chạy cái này sẽ gửi một yêu cầu đến example.com và trả về phản hồi. Nhớ rằng, bạn sẽ cần xử lý kết quả để thực sự sử dụng nó.

## Sâu hơn
Trong lịch sử, việc gửi yêu cầu HTTP là nhiệm vụ cho các công cụ chuyên biệt như `curl` hay thư viện trong các ngôn ngữ khác. Điều này khá mới mẻ khi chính các ngôn ngữ đó đang tích hợp chức năng gửi yêu cầu HTTP một cách mượt mà. Các phương thức thay thế để gửi yêu cầu HTTP trong Gleam bao gồm các thư viện của bên thứ ba như `gleam_http` và bindings cụ thể cho nền tảng.

Về triển khai, có hai phần: xây dựng yêu cầu và nhận phản hồi. Yêu cầu có các phương thức (GET, POST, v.v.), URL, headers, và nội dung trong khi phản hồi mang theo mã trạng thái, headers, và nội dung nữa.

Hệ thống kiểu và kỹ thuật so khớp mẫu của Gleam nổi bật ở đây, cho phép xử lý lỗi toàn diện và việc phân tích phản hồi một cách rõ ràng. Đây không chỉ là việc phóng dữ liệu vào chỗ trống và hy vọng điều tốt nhất; đây là một cuộc trò chuyện được kiểm soát, có cấu trúc.

## Xem thêm
- [Tài liệu HTTP của Gleam](https://hexdocs.pm/gleam_httpc/)
- [HTTP RFC 7231](https://tools.ietf.org/html/rfc7231) cho những thông tin cần biết về HTTP.
- [Thư viện tiêu chuẩn của Gleam](https://hexdocs.pm/gleam_stdlib/) cho các tính năng mạng khác.

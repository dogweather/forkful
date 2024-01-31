---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:07:58.812070-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc truyền tên đăng nhập và mật khẩu để xác nhận danh tính của người dùng. Các lập trình viên thực hiện việc này để truy cập vào những tài nguyên bị hạn chế trên máy chủ, đảm bảo một số mức độ bảo mật.

## Làm thế nào:

Hãy thử nghiệm với một số mã. Chúng ta sẽ sử dụng `curl`, một công cụ dòng lệnh phổ biến. Thay thế `username:password` bằng thông tin xác thực của bạn và `http://example.com/resource` bằng URL mục tiêu của bạn.

```Bash
curl -u username:password http://example.com/resource
```

Hoặc mã hóa thông tin xác thực của bạn bằng base64 trước và sử dụng như sau:

```Bash
# Mã hóa thông tin xác thực
credentials=$(echo -n username:password | base64)

# Gửi yêu cầu
curl -H "Authorization: Basic $credentials" http://example.com/resource
```

Kết quả mẫu cho một yêu cầu thành công có thể trông như thế này:

```Bash
{
  "data": "Một số thông tin bị hạn chế",
  "message": "Truy cập được chấp nhận"
}
```

## Sâu hơn

Theo lịch sử, xác thực cơ bản đã là một phần của HTTP từ những ngày đầu, nhưng nó không phải không có nhược điểm - chủ yếu là sự dễ bị tấn công nếu không sử dụng qua một kênh an toàn như HTTPS.

Các lựa chọn thay thế bao gồm OAuth, được coi là an toàn hơn và cung cấp kiểm soát tốt hơn về những gì được truy cập. Xác thực Digest là một lựa chọn khác, gửi thông tin xác thực được băm thay vì văn bản thuần.

Về cơ học, khi bạn gửi thông tin xác thực cơ bản, chúng được bao gồm trong tiêu đề HTTP được mã hóa trong Base64. Đây không phải là mã hóa, vì vậy nếu bạn không sử dụng HTTPS, bất kỳ ai chặn được yêu cầu đều có thể giải mã một cách dễ dàng. Sử dụng HTTPS bảo vệ việc truyền dữ liệu, mã hóa mọi thứ giữa khách hàng và máy chủ.

## Xem thêm

- Tài liệu chính thức của cURL: https://curl.haxx.se/docs/manpage.html
- Xác thực HTTP: Xác thực Truy cập Cơ bản và Digest (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- Giới thiệu về OAuth: https://oauth.net/2/introduction/

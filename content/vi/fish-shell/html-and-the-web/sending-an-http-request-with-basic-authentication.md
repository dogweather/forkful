---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:02.386410-07:00
description: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c truy\u1EC1n t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u qua m\u1EA1ng \u0111\u1EC3 truy c\u1EADp v\xE0o c\xE1\
  c t\xE0i nguy\xEAn \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7. L\u1EADp\u2026"
lastmod: '2024-03-13T22:44:37.210237-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c truy\u1EC1n t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u qua m\u1EA1ng \u0111\u1EC3 truy c\u1EADp v\xE0o c\xE1\
  c t\xE0i nguy\xEAn \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7. L\u1EADp\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Gì & Tại Sao?

Việc gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc truyền tên người dùng và mật khẩu qua mạng để truy cập vào các tài nguyên được bảo vệ. Lập trình viên sử dụng phương pháp này cho sự đơn giản khi tương tác với các API hoặc dịch vụ đòi hỏi thông tin đăng nhập.

## Cách thực hiện:

Trong Fish Shell, sử dụng `curl` để thực hiện một yêu cầu HTTP với xác thực cơ bản. Thay thế `username`, `password`, và `the_url`:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

Hoặc, để `curl` tự xử lý việc mã hóa:

```Fish Shell
curl -u username:password the_url
```

Kết quả mẫu có thể trông như thế này:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "Authenticated successfully."
}
```

## Sâu hơn

Xác thực cơ bản là một phần của giao thức HTTP, tồn tại từ đầu những năm 90. Mặc dù dễ dàng triển khai, nó kém an toàn hơn do thông tin đăng nhập chỉ được mã hóa base64, không phải được mã hóa. HTTPS giúp ích, nhưng không phải là giải pháp hoàn hảo.

Các phương án thay thế bao gồm OAuth, sử dụng token thay vì thông tin đăng nhập, thêm các lớp bảo mật. Để tăng cường bảo mật, hãy xem xét sử dụng khóa API hoặc JWT (JSON Web Tokens).

Với Fish Shell, chúng tôi đang giao tiếp với `curl`, một công cụ mạnh mẽ hỗ trợ nhiều giao thức và phương thức xác thực khác nhau. Cờ `-u` rất tiện lợi, nhưng tránh mã hóa thông tin đăng nhập; thay vào đó, sử dụng biến môi trường hoặc tệp cấu hình với quyền truy cập thích hợp.

## Xem Thêm:

- Tài liệu cURL: https://curl.se/docs/httpscripting.html
- RFC Xác Thực HTTP Cơ Bản: https://tools.ietf.org/html/rfc7617
- Tài liệu Fish Shell: https://fishshell.com/docs/current/index.html
- Hiểu về JWT: https://jwt.io/introduction/

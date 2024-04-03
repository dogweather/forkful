---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:02.386410-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Fish Shell, s\u1EED d\u1EE5ng `curl`\
  \ \u0111\u1EC3 th\u1EF1c hi\u1EC7n m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1\
  c th\u1EF1c c\u01A1 b\u1EA3n. Thay th\u1EBF `username`, `password`, v\xE0 `the_url`."
lastmod: '2024-03-13T22:44:37.210237-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish Shell, s\u1EED d\u1EE5ng `curl` \u0111\u1EC3 th\u1EF1c hi\u1EC7\
  n m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

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

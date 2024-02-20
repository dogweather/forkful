---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:16:52.234819-07:00
description: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi ch\u1EE9ng\
  \ th\u1EF1c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c bao g\u1ED3m t\xEAn ng\u01B0\
  \u1EDDi d\xF9ng v\xE0 m\u1EADt kh\u1EA9u \u0111\u01B0\u1EE3c m\xE3 h\xF3a d\u01B0\
  \u1EDBi d\u1EA1ng base64 trong header c\u1EE7a y\xEAu c\u1EA7u. L\u1EADp\u2026"
lastmod: 2024-02-19 22:04:56.362296
model: gpt-4-0125-preview
summary: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi ch\u1EE9ng th\u1EF1\
  c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c bao g\u1ED3m t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u \u0111\u01B0\u1EE3c m\xE3 h\xF3a d\u01B0\u1EDBi d\u1EA1\
  ng base64 trong header c\u1EE7a y\xEAu c\u1EA7u. L\u1EADp\u2026"
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Việc gửi một yêu cầu HTTP với chứng thực cơ bản bao gồm việc bao gồm tên người dùng và mật khẩu được mã hóa dưới dạng base64 trong header của yêu cầu. Lập trình viên làm điều này để truy cập vào những tài nguyên cần một hình thức xác thực đơn giản nhằm đảm bảo một mức độ bảo mật nào đó.

## Làm Thế Nào:

Dưới đây là một ví dụ nhanh sử dụng Fetch API của JavaScript:

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Phản hồi mạng không ổn.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Lỗi fetch: ', error));
```

Kết quả mẫu (in ra console):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## Tìm Hiểu Sâu

Trước khi đi sâu, hãy lấy một chút ngữ cảnh. Chứng thực cơ bản là một trong những hình thức bảo mật dịch vụ web đơn giản nhất, gửi thông tin đăng nhập trong headers với mỗi yêu cầu.

Bối Cảnh Lịch Sử:
- Chứng thực HTTP cơ bản là một phương pháp cũ, lần đầu tiên được trình bày trong RFC 7617 từ 2015, thay thế RFC 2617 còn cũ hơn từ 1999.
- Nó được sử dụng rộng rãi do sự đơn giản nhưng không an toàn như HTTPS, vì mã hóa base64 dễ dàng bị đảo ngược.

Các Phương Án Thay Thế:
- OAuth: Một tiêu chuẩn an toàn và phức tạp hơn cho việc ủy quyền truy cập, được sử dụng trong các trường hợp bạn cần cung cấp quyền truy cập mà không chia sẻ thông tin đăng nhập.
- API Keys: Một token đơn lẻ dễ quản lý hơn các giao thức OAuth phức tạp.
- Bearer Tokens: Đặc biệt là JWT (JSON Web Tokens), có thể chứa nhiều thông tin hơn.

Chi Tiết Triển Khai:
- Mã hóa Base64 chuyển đổi chuỗi tên người dùng:mật khẩu thành một chuỗi ký tự dễ truyền đi hơn.
- Luôn đảm bảo kết nối là HTTPS, để ngăn chặn thông tin đăng nhập bị chặn lại.
- Phát triển hiện đại ưa chuộng token và cookie phiên để xác thực, vì chúng an toàn và linh hoạt hơn.

## Xem Thêm

- [Mạng Lưới Nhà Phát Triển Mozilla - Ủy Quyền](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP Basic Auth](https://tools.ietf.org/html/rfc7617)
- [Giới Thiệu về OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [Token Web JSON (JWT)](https://jwt.io/introduction/)

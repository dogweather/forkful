---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:37.469562-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\xE3 c\xF3 m\u1ED9t th\u1EDDi gian,\
  \ tr\u01B0\u1EDBc khi OAuth v\xE0 JWTs chi\u1EBFm l\u0129nh c\u1EA3nh quan, x\xE1\
  c th\u1EF1c c\u01A1 b\u1EA3n l\xE0 l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7u. N\xF3\
  \ v\u1EABn c\xF2n ti\u1EC7n l\u1EE3i cho c\xE1c c\xF4ng\u2026"
lastmod: '2024-04-05T22:50:50.662239-06:00'
model: gpt-4-0125-preview
summary: "\u0110\xE3 c\xF3 m\u1ED9t th\u1EDDi gian, tr\u01B0\u1EDBc khi OAuth v\xE0\
  \ JWTs chi\u1EBFm l\u0129nh c\u1EA3nh quan, x\xE1c th\u1EF1c c\u01A1 b\u1EA3n l\xE0\
  \ l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7u."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách thực hiện:
```typescript
import axios from 'axios';

// mã hóa tên người dùng và mật khẩu của bạn
const token = Buffer.from('yourUsername:yourPassword').toString('base64');
const url = 'https://your.api/endpoint';

// thiết lập yêu cầu HTTP với Axios
axios.get(url, {
  headers: {
    'Authorization': `Basic ${token}`
  }
})
.then(response => {
  console.log(response.data); // đây là kết quả mong đợi của bạn
})
.catch(error => {
  console.error("Oops, có gì đó không ổn!", error);
});
```

Kết quả mẫu:

```
{ "message": "Bạn vào rồi! Chào mừng đến với vùng API bí mật." }
```

## Đào Sâu
Đã có một thời gian, trước khi OAuth và JWTs chiếm lĩnh cảnh quan, xác thực cơ bản là lựa chọn hàng đầu. Nó vẫn còn tiện lợi cho các công cụ nội bộ hoặc Proof of Concepts (PoCs). Ý tưởng rất đơn giản: ghim một tiêu đề với 'Authorization', sử dụng 'Basic ' + một 'username:password' được mã hóa base64. Voilà, bạn đã qua cổng.

Nhưng nó không phải luôn luôn là cầu vồng. Có rủi ro - nếu bạn không sử dụng HTTPS, bạn thực sự đang hô to thông tin đăng nhập của mình. Các lựa chọn khác? OAuth2 tokens, JWTs, API keys - chúng giống như những kiểu mạnh mẽ, kiểu im lặng. Chúng phục vụ mục đích tương tự nhưng với nhiều phức tạp và an toàn hơn.

Khi triển khai xác thực cơ bản trong TypeScript, lựa chọn phổ biến là `axios` hoặc `fetch`. Trong trường hợp của chúng ta, `axios` làm cho thiết lập tiêu đề tùy chỉnh trở nên dễ dàng. Ngoài ra, nó trả về promises, làm cho nó mơ ước với `async/await`.

Hãy nhớ rằng: 'Basic' sẽ sớm bộc lộ tuổi tác của nó trong thế giới web hiện đại nơi HTTPS là điều cần thiết và tiêu chuẩn an toàn cao hơn. Tuy nhiên, cho các mạng nội bộ hoặc nơi mà sự an toàn cao không quan trọng, nó là một giải pháp đơn giản.

## Xem Thêm
Để biết thêm các phương thức xác thực và các phương pháp hay nhất về bảo mật:

- [MDN Web Docs: Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [OWASP Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- Tài liệu chính thức của `axios` về tiêu đề HTTP tùy chỉnh: [Axios Docs](https://axios-http.com/docs/req_config)

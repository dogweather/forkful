---
title:                "Gửi một yêu cầu HTTP"
aliases: - /vi/javascript/sending-an-http-request.md
date:                  2024-01-28T22:08:00.157698-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc gửi một yêu cầu HTTP là cách mã JavaScript của bạn giao tiếp với máy chủ. Việc này được thực hiện để trao đổi dữ liệu, tải nguồn, hoặc gửi dữ liệu lên máy chủ để xử lý.

## Cách thực hiện:

JavaScript sử dụng API `fetch` để gửi các yêu cầu. Dưới đây là cách thực hiện một yêu cầu GET đơn giản:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(phản_hồi => phản_hồi.json())
  .then(json => console.log(json))
  .catch(lỗi => console.error('Lỗi:', lỗi));
```

Đầu ra sẽ là dữ liệu JSON từ URL. Dễ dàng, phải không?

Và đối với một yêu cầu POST:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  phương_thức: 'POST',
  thân: JSON.stringify({
    tiêu_đề: 'foo',
    nội_dung: 'bar',
    mã_người_dùng: 1,
  }),
  tiêu_đề: {
    'Loại nội dung': 'application/json; charset=UTF-8',
  },
})
  .then(phản_hồi => phản_hồi.json())
  .then(json => console.log(json))
  .catch(lỗi => console.error('Lỗi:', lỗi));
```

Điều này gửi dữ liệu mới và xuất ra phản hồi của máy chủ.

## Tìm hiểu sâu hơn

Yêu cầu HTTP đã tồn tại từ khi web ra đời—nghĩ về các biểu mẫu HTML. XMLHttpRequest (XHR) từng là phương pháp được ưa chuộng để gửi yêu cầu trong JavaScript, nhưng nó không tiện lợi.

Nhập `fetch`, một cách tiếp cận hiện đại dựa trên promise, làm cho nó trở nên sạch sẽ và mạnh mẽ hơn. Không giống như XHR, `fetch` xử lý cả yêu cầu và phản hồi trong một API thống nhất và được tích hợp sẵn vào ngôn ngữ, không cần thư viện.

Có phương án khác? Chắc chắn. Thư viện như Axios hoặc Ajax của jQuery vẫn được sử dụng. Chúng cung cấp một số đường viết ngắn và giải pháp cho các tính năng đặc biệt, nhưng `fetch` là bản địa và nói chung là hướng đi phía trước.

Chi tiết triển khai? Nhớ xử lý lỗi, làm việc với các loại phản hồi khác nhau, và ý thức về quy tắc chia sẻ tài nguyên giữa nguồn gốc (CORS).

## Xem thêm

- Tài liệu MDN về API `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Sử dụng promises trong JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- Tìm hiểu về CORS: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS

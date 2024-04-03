---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:13.690397-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch nhanh ch\xF3ng \u0111\u1EC3 t\u1EA3i m\u1ED9t trang s\u1EED d\u1EE5ng Node.js\
  \ v\u1EDBi `node-fetch`."
lastmod: '2024-03-13T22:44:37.151657-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch nhanh ch\xF3ng \u0111\u1EC3 t\u1EA3\
  i m\u1ED9t trang s\u1EED d\u1EE5ng Node.js v\u1EDBi `node-fetch`."
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
Dưới đây là cách nhanh chóng để tải một trang sử dụng Node.js với `node-fetch`:

```Javascript
const fetch = require('node-fetch'); // bạn có thể cần cài đặt cái này trước!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Xuất ra mã nguồn HTML của trang
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

Kết quả mẫu:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Sâu hơn
Truyền thống, việc tải xuống một trang web được thực hiện với XMLHTTPRequest trong trình duyệt hoặc mô-đun `http` trong Node.js. Tuy nhiên, sau ES6, `fetch` API trở thành tiêu chuẩn hiện đại do cú pháp dễ sử dụng và bản chất dựa trên promise của nó.

Các lựa chọn khác bao gồm `axios`, một gói npm phổ biến, xử lý các yêu cầu với chức năng nhiều hơn một chút so với fetch gốc. Đối với các trường hợp sử dụng phức tạp, bạn có thể sử dụng `puppeteer` để thực sự render trang trong một trình duyệt không đầu, hữu ích cho việc xử lý nội dung được render bằng JavaScript.

Khi thực hiện việc tải trang, hãy chú ý đến các khía cạnh như tôn trọng `robots.txt`, xử lý `User-Agent` để tránh bị chặn, và quản lý việc xử lý bất đồng bộ cẩn thận để tránh các rủi ro tiềm ẩn với việc quá tải máy chủ hoặc điều kiện đua.

## Xem Thêm
- MDN Web Docs về API `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Trang GitHub của Axios: https://github.com/axios/axios
- Trang GitHub của Puppeteer: https://github.com/puppeteer/puppeteer
- Một bài viết về các phương pháp hay nhất khi lấy dữ liệu tự động trên web: https://www.scrapingbee.com/blog/web-scraping-best-practices/

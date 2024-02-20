---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:13.690397-07:00
description: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5\
  y HTML, CSS, JavaScript, v\xE0 b\u1EA5t k\u1EF3 d\u1EEF li\u1EC7u n\xE0o kh\xE1\
  c t\u1EA1o n\xEAn trang t\u1EEB m\xE1y ch\u1EE7. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3\u2026"
lastmod: 2024-02-19 22:04:56.360908
model: gpt-4-0125-preview
summary: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5y HTML,\
  \ CSS, JavaScript, v\xE0 b\u1EA5t k\u1EF3 d\u1EEF li\u1EC7u n\xE0o kh\xE1c t\u1EA1\
  o n\xEAn trang t\u1EEB m\xE1y ch\u1EE7. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n vi\u1EC7c n\xE0y \u0111\u1EC3\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Tải xuống một trang web có nghĩa là lấy HTML, CSS, JavaScript, và bất kỳ dữ liệu nào khác tạo nên trang từ máy chủ. Lập trình viên thực hiện việc này để phân tích nội dung, tự động hóa tương tác, hoặc lưu trữ các trang web.

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

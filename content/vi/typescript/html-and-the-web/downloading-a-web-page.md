---
title:                "Tải trang web"
aliases: - /vi/typescript/downloading-a-web-page.md
date:                  2024-01-28T21:59:59.604263-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web có nghĩa là truy xuất HTML, CSS và có thể là các tài nguyên khác từ URL mà bạn truy cập. Lập trình viên thực hiện việc này để xử lý nội dung, kéo dữ liệu, kiểm tra các bản cập nhật, hoặc để lưu trữ trang web cho việc sử dụng ngoại tuyến.

## Làm thế nào:

Bạn có thể tải một trang web trong TypeScript sử dụng Node.js và thư viện `node-fetch`. Dưới đây là cách làm:

```TypeScript
import fetch from 'node-fetch';

async function downloadWebPage(url: string): Promise<void> {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Điều này in nội dung HTML ra console
    } catch (error) {
        console.error('Tải xuống thất bại:', error);
    }
}

// Sử dụng hàm
downloadWebPage('https://example.com');
```

Mẫu đầu ra (được ngắn gọn):
```
<!doctype html>
<html>
<head>
    <title>Ví dụ tên miền</title>
...
</html>
```

## Thảo luận sâu hơn

Trong quá khứ, việc tải nội dung web được thực hiện qua các công cụ như `wget` hoặc `curl` trong môi trường dòng lệnh. Tuy nhiên, trong lập trình hiện đại, chúng ta có các thư viện như `node-fetch`, `axios`, hoặc `request` (đã lỗi thời nhưng vẫn được sử dụng) cung cấp nhiều chức năng hơn và dễ dàng tích hợp vào ứng dụng JavaScript/TypeScript của mình hơn.

Khi tải một trang web, không chỉ có HTML. CSS, JavaScript, hình ảnh, và các tài sản khác là một phần của nó. Thông thường, chỉ HTML được truy xuất trước, và sau đó bất kỳ xử lý hoặc tải xuống bổ sung nào được quyết định bởi những gì bạn cần từ trang web.

Về mặt thực hiện, `node-fetch` cơ bản là API fetch của window cho Node.js. Nó trả về một promise giải quyết phản hồi của yêu cầu, cho phép bạn nhận được một luồng văn bản (.text()), một đối tượng JSON (.json()), hoặc thậm chí là một bộ đệm (.buffer()) cho dữ liệu nhị phân.

Hãy nhớ rằng quyền truy cập dữ liệu web được quy định bởi tệp `robots.txt` và điều khoản dịch vụ của một trang web. Luôn chắc chắn rằng bạn được phép kéo dữ liệu từ trang và tuân thủ giới hạn tốc độ để tránh vấn đề pháp lý hoặc bị cấm IP.

## Xem Thêm

- [Tài liệu `node-fetch`](https://github.com/node-fetch/node-fetch)
- [MDN Web Docs về Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Thư viện `axios`](https://github.com/axios/axios)
- [Mã trạng thái HTTP (để xử lý phản hồi)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Tính hợp pháp của việc truy cập web](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)

---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:44.938671-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết một tệp văn bản trong JavaScript thường nghĩa là tạo và lưu dữ liệu vào một tệp ở định dạng dễ đọc cho con người. Lập trình viên thực hiện điều này để lưu giữ dữ liệu, như cài đặt, nhật ký, hoặc đầu ra người dùng.

## Làm thế nào:

JavaScript trong trình duyệt không có quyền truy cập trực tiếp vào hệ thống tệp vì lý do an ninh. Nhưng bạn có thể tạo một tệp văn bản và nhắc người dùng lưu nó:

```javascript
function downloadTextFile(text, filename) {
  const blob = new Blob([text], { type: 'text/plain' });
  const a = document.createElement('a');
  a.download = filename;
  a.href = window.URL.createObjectURL(blob);
  a.dataset.downloadurl = ['text/plain', a.download, a.href].join(':');
  a.style.display = "none";
  document.body.appendChild(a);  // Thêm thẻ neo vào thân trang.
  a.click();
  
  document.body.removeChild(a);  // Dọn dẹp thẻ neo sau khi sử dụng.
  window.URL.revokeObjectURL(a.href);  // Giải phóng URL của blob.
}

// Sử dụng:
downloadTextFile('Xin chào, thế giới!', 'example.txt');
```

Node.js cung cấp một cách đơn giản hơn để viết tệp qua mô-đun `fs`:

```javascript
const fs = require('fs');

fs.writeFile('example.txt', 'Xin chào, thế giới!', (err) => {
  if (err) throw err;
  console.log('Đã lưu tệp!');
});
```

## Đi sâu hơn

Trước đây, JavaScript bị giới hạn trong trình duyệt mà không có quyền truy cập vào hệ thống tệp. Node.js đã thay đổi cục diện này bằng cách hé lộ khả năng phía máy chủ.

Các lựa chọn thay thế cho `fs.writeFile` bao gồm `fs.writeFileSync` cho các hoạt động đồng bộ và `fs.promises.writeFile` cho kiểm soát không đồng bộ dựa trên promise.

Các phương thức `fs` của Node xử lý bộ đệm và luồng - các công cụ giải quyết vấn đề xử lý tệp lớn và giao tiếp mạng.

## Xem thêm

- Tài liệu Hệ thống Tệp Node.js: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN - Blob: [https://developer.mozilla.org/en-US/docs/Web/API/Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- Hướng dẫn JavaScript trên MDN: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)

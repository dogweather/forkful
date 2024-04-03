---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:44.938671-07:00
description: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong JavaScript\
  \ th\u01B0\u1EDDng ngh\u0129a l\xE0 t\u1EA1o v\xE0 l\u01B0u d\u1EEF li\u1EC7u v\xE0\
  o m\u1ED9t t\u1EC7p \u1EDF \u0111\u1ECBnh d\u1EA1ng d\u1EC5 \u0111\u1ECDc cho con\
  \ ng\u01B0\u1EDDi. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
lastmod: '2024-03-13T22:44:37.177258-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong JavaScript\
  \ th\u01B0\u1EDDng ngh\u0129a l\xE0 t\u1EA1o v\xE0 l\u01B0u d\u1EEF li\u1EC7u v\xE0\
  o m\u1ED9t t\u1EC7p \u1EDF \u0111\u1ECBnh d\u1EA1ng d\u1EC5 \u0111\u1ECDc cho con\
  \ ng\u01B0\u1EDDi."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

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

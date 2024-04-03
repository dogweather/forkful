---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:47.450584-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1\
  n \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong JavaScript hi\u1EC7n t\u1EA1\
  i: **S\u1EED d\u1EE5ng Node.js v\u1EDBi Promises (Async/Await)**."
lastmod: '2024-03-13T22:44:37.176002-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n \u0111\u1ECDc m\u1ED9\
  t t\u1EC7p v\u0103n b\u1EA3n trong JavaScript hi\u1EC7n t\u1EA1i."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm thế nào:
Dưới đây là cách bạn đọc một tệp văn bản trong JavaScript hiện tại:

**Sử dụng Node.js với Promises (Async/Await)**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('Có lỗi khi đọc tệp:', error);
  }
}

readFile('example.txt');
```

Đầu ra mẫu (nội dung của `example.txt`):

```
Xin chào, đây là một tệp văn bản!
```

**Sử dụng API fetch trong trình duyệt**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('Ồ, đã có lỗi xảy ra khi tải tệp:', error);
  }
}

fetchTextFile('example.txt');
```

## Sâu hơn nữa
Ban đầu, việc đọc tệp trong JavaScript phần lớn là một việc của máy chủ, được xử lý bởi Node.js. Khi JS tiến vào trình duyệt với HTML5, các API như `FileReader` và `fetch` đã xuất hiện, làm cho việc đọc tệp ở phía máy khách trở nên khả thi mà không cần phải vất vả.

Có các phương án khác? Ồ, có một vài. Streams có thể xử lý các tệp lớn mà không lấn chiếm bộ nhớ. Các Workers ngăn chặn sự đóng băng của giao diện người dùng. Thư viện làm cho các nhiệm vụ phức tạp trở nên dễ dàng hơn. Mỗi cái đều có chỗ đứng của nó.

Bên trong, việcc đọc tệp có thể liên quan đến quản lý bộ nhớ đệm, mã hóa ký tự (UTF-8, v.v.), và xử lý lỗi. Hãy lưu ý đến bảo mật; trình duyệt hạn chế quyền truy cập tệp vì những lý do chính đáng.

## Xem thêm
Mở rộng kiến thức của bạn với những tài nguyên này:

- Tài liệu API FileReader của MDN: [MDN FileReader](https://developer.mozilla.org/vi/docs/Web/API/FileReader)
- Tài liệu Hệ thống Tệp Node.js: [Node.js fs](https://nodejs.org/api/fs.html)
- API Stream cho các tệp lớn: [Node.js stream](https://nodejs.org/api/stream.html)
- Hiểu biết về fetch API: [MDN fetch](https://developer.mozilla.org/vi/docs/Web/API/Fetch_API)

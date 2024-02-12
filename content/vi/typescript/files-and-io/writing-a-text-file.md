---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:13:13.200611-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc viết một tệp văn bản có nghĩa là lưu dữ liệu vào một tệp có phần mở rộng `.txt`. Lập trình viên thực hiện điều này để log, cấu hình, hoặc lưu trữ dữ liệu đơn giản mà không cần đến cơ sở dữ liệu.

## Cách thực hiện:

TypeScript, tuy là một bộ mở rộng của JavaScript, không có module hệ thống tệp của riêng mình, nhưng nó có thể sử dụng Node.js cho nhiệm vụ này. Đảm bảo bạn đã cài đặt Node.js và sau đó, chúng ta bắt đầu:

```typescript
// Nhập module 'fs' để tương tác với hệ thống tệp
import { writeFile } from 'fs';

// Nội dung bạn muốn viết
const content = 'Hello, World!';

// Hàm để viết nội dung vào tệp
const writeTextToFile = (filePath: string, content: string): void => {
  writeFile(filePath, content, (err) => {
    if (err) {
      console.error('Error writing file:', err);
    } else {
      console.log('File written successfully');
    }
  });
};

// Sử dụng hàm để viết vào 'output.txt'
writeTextToFile('./output.txt', content);
```

Kết quả mẫu:
```
File written successfully
```

## Sâu hơn

Lịch sử, việc ghi vào tệp văn bản cũ kỹ như chính bản thân việc tính toán để lưu trữ hoặc giao tiếp giữa các chương trình. Trước khi cơ sở dữ liệu trở nên phổ biến, tệp phẳng là điều thường thấy. Bây giờ, cơ sở dữ liệu đã đảm nhận vai trò này một cách lớn, nhưng tệp văn bản vẫn quan trọng với sự đơn giản của chúng.

Các phương án thay thế cho module 'fs' của Node.js bao gồm:

- 'fs/promises' mới cho các chức năng dựa trên Promise.
- Sử dụng 'fs-extra' để có các phương thức tiện lợi.
- Mô-đun 'stream' để xử lý các tệp lớn.

Phương thức 'writeFile' được trình bày ở trên hoạt động tốt cho tệp từ nhỏ đến trung bình. Đối với các tệp lớn hơn hoặc luồng dữ liệu, bạn có thể muốn sử dụng luồng để tránh tải mọi thứ vào bộ nhớ.

## Xem thêm

- API Hệ thống tệp Node.js: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Trang chính thức của TypeScript: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- Thư viện 'fs-extra': [https://github.com/jprichardson/node-fs-extra](https://github.com/jprichardson/node-fs-extra)
- MDN Web Docs về Streams: [https://developer.mozilla.org/en-US/docs/Web/API/Streams_API](https://developer.mozilla.org/en-US/docs/Web/API/Streams_API)

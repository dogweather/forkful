---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.711300-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103\
  n b\u1EA3n trong TypeScript s\u1EED d\u1EE5ng module `fs/promises` c\u1EE7a Node.js.\
  \ Ch\xFAng t\xF4i s\u1EBD gi\u1EEF v\xED d\u1EE5 n\xE0y \u0111\u01B0\u1EE3c \u0111\
  \u01A1n gi\u1EA3n: \u0111\u1ECDc m\u1ED9t t\u1EC7p c\xF3\u2026"
lastmod: '2024-03-13T22:44:36.342403-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong TypeScript\
  \ s\u1EED d\u1EE5ng module `fs/promises` c\u1EE7a Node.js."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm thế nào:
Hãy đọc một tệp văn bản trong TypeScript sử dụng module `fs/promises` của Node.js. Chúng tôi sẽ giữ ví dụ này được đơn giản: đọc một tệp có tên là `example.txt` và ghi lại nội dung của nó.

```typescript
import { readFile } from 'fs/promises';

async function readTextFile(filePath: string) {
  try {
    const data = await readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error(`Error reading file from disk: ${error}`);
  }
}

readTextFile('./example.txt');
```

Kết quả Mẫu:
```
Hello, this is content from the file!
```

## Tìm hiểu kỹ lưỡng
Trong lịch sử, việc đọc tệp trong Node.js chủ yếu dựa vào callback, có thể dẫn đến hiện tượng được biết đến là "địa ngục callback". Với sự ra đời của Promises và `async/await`, quy trình này trở nên thuận tiện hơn nhiều.

Bên cạnh `fs/promises`, có module `fs` cũ hơn vẫn sử dụng mô hình callback. Cũng có tùy chọn sử dụng xử lý luồng với `fs.createReadStream()`, hữu ích cho các tệp lớn do tiêu thụ ít bộ nhớ hơn.

Về mặt triển khai, truy cập hệ thống tệp là một hoạt động I/O và về cơ bản chậm hơn các thao tác trong bộ nhớ. Đó là lý do tại sao các mô hình mã hóa bất đồng bộ là quan trọng - chúng giúp ngăn chặn việc chặn luồng chính và cho phép Node.js tiếp tục xử lý các nhiệm vụ khác.

## Xem thêm
Để tìm hiểu sâu hơn về hệ thống tệp của Node.js:
- Node.js fs tài liệu: https://nodejs.org/api/fs.html
- Hiểu về `fs/promises`: https://nodejs.org/dist/latest/docs/api/fs.html#filehandlepromises
- Đọc tệp dựa trên luồng: https://nodejs.org/api/stream.html#stream

Dành cho nguồn tài nguyên cụ thể của TypeScript:
- TypeScript Deep Dive: https://basarat.gitbook.io/typescript/
- Sổ tay TypeScript: https://www.typescriptlang.org/docs/handbook/intro.html

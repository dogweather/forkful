---
aliases:
- /vi/typescript/reading-a-text-file/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.711300-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y n\u1ED9i dung t\u1EEB m\u1ED9t t\u1EC7p \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc nh\u01B0\
  \ v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ ho\u1EB7c ph\xE2n\u2026"
lastmod: 2024-02-18 23:08:50.434482
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y n\u1ED9i dung t\u1EEB m\u1ED9t t\u1EC7p \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc nh\u01B0\
  \ v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ ho\u1EB7c ph\xE2n\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?

Đọc một tệp văn bản là việc lấy nội dung từ một tệp được cấu trúc như văn bản dễ đọc cho con người. Lập trình viên thực hiện điều này để xử lý hoặc phân tích dữ liệu, như đọc cấu hình, nhập dữ liệu, hoặc đơn giản là thu thập nội dung để xử lý bởi một ứng dụng.

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

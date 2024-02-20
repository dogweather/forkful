---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:09.488181-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0\
  \ t\u1EA1o m\u1ED9t t\u1EC7p ch\u1EC9 c\u1EA7n thi\u1EBFt trong m\u1ED9t th\u1EDD\
  i gian ng\u1EAFn, th\u01B0\u1EDDng l\xE0 trong qu\xE1 tr\xECnh th\u1EF1c thi c\u1EE7\
  a m\u1ED9t ch\u01B0\u01A1ng tr\xECnh. L\u1EADp tr\xECnh\u2026"
lastmod: 2024-02-19 22:04:55.495939
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t t\u1EC7p ch\u1EC9 c\u1EA7n thi\u1EBFt trong m\u1ED9t th\u1EDDi gian ng\u1EAF\
  n, th\u01B0\u1EDDng l\xE0 trong qu\xE1 tr\xECnh th\u1EF1c thi c\u1EE7a m\u1ED9t\
  \ ch\u01B0\u01A1ng tr\xECnh. L\u1EADp tr\xECnh\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tệp tạm thời có nghĩa là tạo một tệp chỉ cần thiết trong một thời gian ngắn, thường là trong quá trình thực thi của một chương trình. Lập trình viên thực hiện việc này cho các tác vụ như lưu trữ dữ liệu quá lớn cho bộ nhớ, chia sẻ thông tin giữa các quá trình, hoặc lưu trạng thái trong các hoạt động phức tạp.

## Cách thức:
Tạo một tệp tạm thời trong TypeScript không được tích hợp sẵn, nhưng bạn có thể sử dụng module `fs` trong Node.js để thực hiện công việc này. Dưới đây là cách đơn giản để tạo và sử dụng một tệp tạm thời.

```typescript
import { mkdtempSync, writeFileSync, readFileSync, unlinkSync } from 'fs';
import { join } from 'path';

// Tạo một thư mục tạm thời để chứa tệp
const tmpDir = mkdtempSync(join(process.cwd(), 'temp-'));

// Định nghĩa đường dẫn tệp tạm thời
const tmpFilePath = join(tmpDir, 'temp-file.txt');

// Viết một số dữ liệu vào tệp tạm thời
writeFileSync(tmpFilePath, 'Dữ liệu tạm thời');

// Đọc dữ liệu trở lại từ tệp
const data = readFileSync(tmpFilePath, 'utf-8');
console.log(data); // Kết quả: Dữ liệu tạm thời

// Dọn dẹp: xóa tệp tạm thời
unlinkSync(tmpFilePath);
```

Đoạn mã này thiết lập một tệp tạm thời, viết vào đó, đọc từ nó, và sau đó dọn dẹp bằng cách xóa nó.

## Sâu hơn
Khái niệm về tệp tạm thời không phải là mới; chúng đã tồn tại từ những ngày đầu tiên của lập trình. Tệp tạm thời trên các hệ thống giống Unix thường được tạo trong `/tmp` hoặc `/var/tmp`, và Windows sử dụng `%TEMP%`. Trong các hệ thống an toàn hoặc có khả năng mở rộng hơn, bạn có thể sử dụng một cơ sở dữ liệu hoặc một dịch vụ như Redis để lưu trữ dữ liệu tạm thời.

Trong TypeScript, chúng tôi thường phụ thuộc vào module `fs` của Node.js, như đã trình bày ở trên, nhưng có những thư viện như `tmp` cung cấp các tính năng nâng cao và xử lý dọn dẹp tự động. Sử dụng thư mục tạm thời của hệ thống có thể rủi ro vì có thể xảy ra va chạm tên hoặc các vấn đề về bảo mật. Vì vậy, luôn đảm bảo bạn xử lý việc tạo và phá hủy tệp một cách cẩn thận để tránh xung đột và rò rỉ. Thêm vào đó, việc đặt tên duy nhất, như được cung cấp bởi các thư viện như `uuid`, có thể giúp ngăn chặn các va chạm.

Một lựa chọn khác cho tệp tạm thời vật lý là sử dụng các hệ thống tệp trong bộ nhớ, như `memfs`. Điều này tránh được I/O đĩa và có thể tăng tốc các hoạt động cần bộ nhớ tạm thời, nhưng nó bị giới hạn bởi bộ nhớ hệ thống.

Hãy nhớ, khi sử dụng tệp tạm thời, hãy cẩn thận với dữ liệu nhạy cảm. Tệp tạm thời thường kém an toàn hơn và có thể được các quá trình hoặc người dùng khác truy cập, đặc biệt trên các hệ thống chia sẻ.

## Xem thêm
- Module Hệ thống Tệp của Node.js: https://nodejs.org/api/fs.html
- Thư viện `tmp` cho việc xử lý tệp tạm thời tiên tiến hơn: https://www.npmjs.com/package/tmp
- Thư viện `uuid` để tạo tên duy nhất: https://www.npmjs.com/package/uuid
- Thư viện hệ thống tệp trong bộ nhớ `memfs`: https://www.npmjs.com/package/memfs
- Tài liệu chính thức của TypeScript: https://www.typescriptlang.org/docs/

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:31.562503-07:00
description: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng l\xE0 \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng folder th\u1EF1c\
  \ s\u1EF1 c\xF3 m\u1EB7t tr\u01B0\u1EDBc khi b\u1EA1n \u0111\u1ECDc t\u1EEB n\xF3\
  \ ho\u1EB7c ghi v\xE0o n\xF3. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.338586-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4\
  ng l\xE0 \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng folder th\u1EF1c s\u1EF1\
  \ c\xF3 m\u1EB7t tr\u01B0\u1EDBc khi b\u1EA1n \u0111\u1ECDc t\u1EEB n\xF3 ho\u1EB7\
  c ghi v\xE0o n\xF3."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cách thực hiện:
Trong TypeScript, bạn thường sử dụng module `fs` của Node.js để kiểm tra một thư mục. Đây là cách nhanh để làm điều đó:

```typescript
import { existsSync } from 'fs';

// Kiểm tra xem một thư mục có tồn tại không
const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log(`Vâng, nó ở đây!`);
} else {
  console.log(`Không, nó không tồn tại.`);
}
```

Kết quả phụ thuộc vào việc thư mục có tồn tại hay không:
```
Vâng, nó ở đây!
// hoặc
Không, nó không tồn tại.
```

## Sâu hơn nữa
Trước đây, mọi người sử dụng `fs.exists` không đồng bộ, nhưng nó đã bị loại bỏ vì có thói quen gây ra các lỗi lập trình, như tình trạng đua check-then-act. `existsSync` đơn giản hơn và loại bỏ hẳn vấn đề về callback.

Về các phương án thay thế, các phương thức `fs.statSync` hoặc `fs.accessSync` cũng có thể đảm nhận công việc này nhưng yêu cầu một chút mã code nhiều hơn:

```typescript
import { statSync } from 'fs';

try {
  const stats = statSync(directoryPath);
  if (stats.isDirectory()) {
    console.log('Quả thực nó tồn tại.');
  }
} catch (error) {
  if (error.code === 'ENOENT') {
    console.log('Không, không tìm thấy nó đâu.');
  }
}
```

Cả `statSync` và `accessSync` đều sẽ ném ra lỗi nếu đường dẫn không tồn tại, do đó bạn cần phải xử lý điều đó.

Khi sử dụng TypeScript, nhớ rằng những phương thức này đến từ Node.js, không phải là TypeScript. Và vai trò của TypeScript? Chủ yếu, nó chỉ cung cấp các kiểu dữ liệu và đảm bảo bạn sử dụng các phương thức một cách chính xác.

## Xem thêm
- Tài liệu Hệ thống Tệp của Node.js: https://nodejs.org/api/fs.html
- Sổ tay TypeScript: https://www.typescriptlang.org/docs/handbook/intro.html
- Xử lý Lỗi trong Node.js: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/

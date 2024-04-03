---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:32.501734-07:00
description: "C\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p ng\u01B0\u1EDD\
  i d\xF9ng truy\u1EC1n d\u1EEF li\u1EC7u v\xE0o ch\u01B0\u01A1ng tr\xECnh khi h\u1ECD\
  \ ch\u1EA1y n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng \u0111\
  \u1EC3 t\xF9y ch\u1EC9nh h\xE0nh vi c\u1EE7a ch\u01B0\u01A1ng\u2026"
lastmod: '2024-03-13T22:44:36.339889-06:00'
model: gpt-4-0125-preview
summary: "C\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p ng\u01B0\u1EDD\
  i d\xF9ng truy\u1EC1n d\u1EEF li\u1EC7u v\xE0o ch\u01B0\u01A1ng tr\xECnh khi h\u1ECD\
  \ ch\u1EA1y n\xF3."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Trong TypeScript, bạn sử dụng Node.js để đọc các đối số dòng lệnh. Dưới đây là cách:

```typescript
// Cần nhập process từ Node.js
import process from 'process';

// Lấy các đối số dòng lệnh từ vị trí thứ ba trở đi
const args = process.argv.slice(2);

console.log('Đối số dòng lệnh:', args);
```

Chạy script này như `ts-node yourscript.ts arg1 arg2` và xem:

```
Đối số dòng lệnh: ['arg1', 'arg2']
```

## Đi sâu hơn
Trở lại những ngày đầu dòng lệnh, tương tác người dùng chỉ xoay quanh văn bản. Linux, UNIX, và Windows sử dụng đối số dòng lệnh để bảo chương trình phải làm gì.

Bây giờ để xem các phương án thay thế: ngoài `process.argv`, trong Node.js, bạn có thể sử dụng các thư viện như `yargs` hoặc `commander` để có thêm các tính năng như phân tích và xác nhận.

Bản chất của việc này trong TypeScript là đơn giản: `process.argv` là một mảng với tất cả các đối số. Chỉ mục 0 là đường dẫn đến Node, chỉ mục 1 là đường dẫn của script, nên các đối số thực sự bắt đầu từ chỉ mục 2.

## Xem thêm
Để khám phá thêm, bắt đầu với những điều này:

- [Tài liệu về process.argv của Node.js](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Kho lưu trữ GitHub của Yargs](https://github.com/yargs/yargs)
- [Kho lưu trữ GitHub của Commander.js](https://github.com/tj/commander.js)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:32.501734-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong TypeScript, b\u1EA1n s\u1EED d\u1EE5ng\
  \ Node.js \u0111\u1EC3 \u0111\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7\
  nh. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch."
lastmod: '2024-03-13T22:44:36.339889-06:00'
model: gpt-4-0125-preview
summary: "Trong TypeScript, b\u1EA1n s\u1EED d\u1EE5ng Node.js \u0111\u1EC3 \u0111\
  \u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh."
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

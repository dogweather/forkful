---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:32.501734-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Các đối số dòng lệnh cho phép người dùng truyền dữ liệu vào chương trình khi họ chạy nó. Các lập trình viên sử dụng chúng để tùy chỉnh hành vi của chương trình mà không cần thay đổi mã.

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

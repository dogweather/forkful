---
title:                "Đọc các đối số dòng lệnh"
aliases: - /vi/javascript/reading-command-line-arguments.md
date:                  2024-01-28T22:05:41.012108-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Đọc các đối số dòng lệnh có nghĩa là lấy các bổ sung mà người dùng thêm vào các lệnh khi họ chạy script của bạn. Lập trình viên làm điều này để cho phép người dùng tùy chỉnh hành vi mà không cần thay đổi mã.

## Làm thế nào:
Dưới đây là cách thẳng thắn để làm điều đó trong Node.js:

```javascript
// process.argv chứa các đối số dòng lệnh
const args = process.argv.slice(2);

console.log(args);

// Chạy script này với: node yourscript.js firstArg secondArg
```

Kết quả mẫu nếu bạn chạy `node yourscript.js pineapple 42`:

```javascript
['pineapple', '42']
```

Việc sử dụng một package như `yargs` sẽ làm cho cuộc sống dễ dàng hơn, cho phép bạn định nghĩa và truy cập các đối số theo tên.

```javascript
// Cài đặt với npm install yargs
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// Chạy điều này với: node yourscript.js --fruit pineapple --number 42
```

Và bạn sẽ nhận được:

```javascript
{ fruit: 'pineapple', number: '42' }
```

Rõ ràng và sạch sẽ, với các tham số được đặt tên.

## Sâu hơn
Ngày xưa, các đối số được đọc trong C sử dụng `argc` và `argv` trong hàm `main`. Trong Node.js, `process.argv` là lựa chọn hàng đầu. Đó là một mảng nơi phần tử đầu tiên là đường dẫn đến file thực thi node, phần tử thứ hai là tên file script, và các phần còn lại là các đối số thực tế của bạn.

`yargs` là một lựa chọn tuyệt vời cho các ứng dụng phức tạp: nó phân tích các đối số thành một đối tượng tiện lợi, quản lý các giá trị mặc định, và thậm chí còn tự động tạo ra các thông điệp trợ giúp.

Còn có package `minimist`, một lựa chọn nhẹ nhàng hơn `yargs`, nếu bạn hướng tới sự tối giản.

Sâu thẳm, Node.js sử dụng `process.binding('options')` của V8 để phân tích mà không được tiết lộ với người dùng trung bình. Phương thức nội bộ này đóng gói rất nhiều tiện ích bên dưới, quản lý việc phân tích và truy xuất các tuỳ chọn dòng lệnh.

## Xem thêm
- Tài liệu Node.js process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Kho GitHub của Yargs: https://github.com/yargs/yargs
- Minimist trên npm: https://www.npmjs.com/package/minimist

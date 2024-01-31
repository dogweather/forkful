---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:56:11.890825-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc kiểm tra sự tồn tại của một thư mục là để xác nhận liệu có một thư mục nào đó tồn tại tại một đường dẫn cụ thể trong hệ thống tệp hay không. Lập trình viên thực hiện điều này để tránh các lỗi như cố gắng đọc từ hoặc viết vào một thư mục không tồn tại.

## Làm thế nào:
Trong JavaScript (chạy trong môi trường Node.js), có một mô-đun tích hợp gọi là `fs` mà bạn có thể sử dụng để kiểm tra xem thư mục có tồn tại hay không. Dưới đây là một ví dụ nhanh:

```javascript
const fs = require('fs');
const path = './path/to/directory';

fs.access(path, fs.constants.F_OK, (err) => {
    if (err) {
        console.error(`${path} không tồn tại`);
    } else {
        console.log(`${path} tồn tại`);
    }
});
```

Kết quả mẫu:

```
./path/to/directory tồn tại
```

Hoặc sử dụng API `fs.promises` mới hơn với async/await:

```javascript
const fs = require('fs').promises;

async function checkDirectoryExists(path) {
    try {
        await fs.access(path, fs.constants.F_OK);
        console.log(`${path} tồn tại`);
    } catch {
        console.error(`${path} không tồn tại`);
    }
}

checkDirectoryExists('./path/to/directory');
```

Kết quả mẫu:

```
./path/to/directory không tồn tại
```

## Sâu hơn
Trong lịch sử, việc kiểm tra một tệp hoặc thư mục bao gồm việc sử dụng `fs.stat` hoặc `fs.existsSync`, nhưng các phương pháp này có nhược điểm. `fs.stat` đòi hỏi phải có thêm logic để xác định liệu đường dẫn có phải là thư mục không, và `fs.existsSync` là đồng bộ, có thể chặn vòng lặp sự kiện trong Node.js.

Một phương án khác là sử dụng API `fs.promises` hoặc async/await để cải thiện khả năng đọc và giữ cho chương trình của bạn không bị chặn.

Một chi tiết thực hiện là `fs.access` chỉ kiểm tra sự tồn tại, không kiểm tra khả năng đọc hoặc ghi của thư mục. Có thể sử dụng các cờ khác với `fs.access` để kiểm tra những quyền này nếu cần.

## Xem thêm
- Tài liệu Node.js `fs`: [Mô-đun fs của Node.js](https://nodejs.org/api/fs.html)
- Thêm thông tin về async/await: [Hàm bất đồng bộ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Thông tin về cờ hệ thống tệp: [Cờ Hệ Thống Tệp](https://nodejs.org/api/fs.html#file-system-flags)

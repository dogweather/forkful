---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:11.890825-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong JavaScript (ch\u1EA1y trong m\xF4i tr\u01B0\
  \u1EDDng Node.js), c\xF3 m\u1ED9t m\xF4-\u0111un t\xEDch h\u1EE3p g\u1ECDi l\xE0\
  \ `fs` m\xE0 b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng \u0111\u1EC3 ki\u1EC3m tra\
  \ xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i\u2026"
lastmod: '2024-03-13T22:44:37.172269-06:00'
model: gpt-4-0125-preview
summary: "Trong JavaScript (ch\u1EA1y trong m\xF4i tr\u01B0\u1EDDng Node.js), c\xF3\
  \ m\u1ED9t m\xF4-\u0111un t\xEDch h\u1EE3p g\u1ECDi l\xE0 `fs` m\xE0 b\u1EA1n c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng \u0111\u1EC3 ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3\
  \ t\u1ED3n t\u1EA1i hay kh\xF4ng."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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

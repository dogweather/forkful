---
aliases:
- /vi/typescript/using-a-debugger/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:19.502320-07:00
description: "M\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i l\xE0 c\xF4ng c\u1EE5 cho ph\xE9\
  p b\u1EA1n xem x\xE9t v\xE0 thay \u0111\u1ED5i n\u1ED9i dung b\xEAn trong m\xE3\
  \ c\u1EE7a m\xECnh khi n\xF3 \u0111ang ch\u1EA1y. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng n\xF3 \u0111\u1EC3 t\xECm v\xE0 lo\u1EA1i b\u1ECF\u2026"
lastmod: 2024-02-18 23:08:50.420386
model: gpt-4-0125-preview
summary: "M\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i l\xE0 c\xF4ng c\u1EE5 cho ph\xE9p b\u1EA1\
  n xem x\xE9t v\xE0 thay \u0111\u1ED5i n\u1ED9i dung b\xEAn trong m\xE3 c\u1EE7a\
  \ m\xECnh khi n\xF3 \u0111ang ch\u1EA1y. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng n\xF3 \u0111\u1EC3 t\xECm v\xE0 lo\u1EA1i b\u1ECF\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Một trình gỡ lỗi là công cụ cho phép bạn xem xét và thay đổi nội dung bên trong mã của mình khi nó đang chạy. Lập trình viên sử dụng nó để tìm và loại bỏ lỗi bằng cách bước qua mã của họ, kiểm tra các biến, và hiểu luồng của chương trình.

## Cách thực hiện:

Để bắt đầu với trình gỡ lỗi trong TypeScript, tất cả những gì bạn cần là một IDE được hỗ trợ (như Visual Studio Code) và một cấu hình `launch.json`. Dưới đây là một ví dụ nhanh cho một ứng dụng Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Để gỡ lỗi, tạo một tệp `launch.json` dưới thư mục `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Khởi động Chương trình",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Sau đó, đặt một điểm dừng trong hàm `greet` của bạn bằng cách nhấp vào phía bên trái của số dòng trong IDE của bạn. Nhấn F5 để bắt đầu gỡ lỗi, và xem ứng dụng của bạn tạm dừng tại điểm dừng. Bây giờ bạn có thể di chuột qua các biến, xem các biểu thức, và bước qua mã của mình một cách dễ dàng.

## Sâu hơn

Trở lại thời gian trước khi các môi trường phát triển tích hợp (IDEs) trở nên mạnh mẽ, việc gỡ lỗi thường được thực hiện với các lệnh in (còn được biết đến là gỡ lỗi bằng `console.log`). Nó hoạt động, dạng nào đó, nhưng giống như việc cố gắng tìm một cái kim trong đống rơm khi bị bịt mắt.

Các trình gỡ lỗi hiện đại giống như một công cụ đa năng cho việc khắc phục sự cố. Với sự phát triển của TypeScript và Node.js, có nhiều trình gỡ lỗi khác nhau, từ trình kiểm tra tích hợp của Node.js đến công cụ phát triển trình duyệt cho việc gỡ lỗi phía client.

Trình kiểm tra của Node.js hoạt động bằng cách gắn vào ứng dụng đang chạy của bạn; nó giao tiếp qua Giao thức Chrome DevTools, biến trình duyệt Chrome của bạn thành một bảng điều khiển gỡ lỗi mạnh mẽ. Sự tích hợp này cho phép một phiên gỡ lỗi rõ ràng, tương tác hình ảnh và chi tiết, so với các phương pháp gỡ lỗi dòng lệnh truyền thống.

## Xem thêm

Để đọc thêm và một số mẹo chuyên nghiệp, hãy kiểm tra:

- [Gỡ lỗi TypeScript trong Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Hướng dẫn Gỡ lỗi Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Tài liệu Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)

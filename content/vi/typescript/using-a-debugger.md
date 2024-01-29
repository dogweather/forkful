---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:10:19.502320-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

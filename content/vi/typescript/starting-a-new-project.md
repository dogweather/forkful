---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:40.295460-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Bắt đầu một dự án mới bằng TypeScript chính là việc thiết lập một nền tảng vững chắc để lập trình. Các lập trình viên khởi xướng dự án mới để biến những ý tưởng mới thành phần mềm hoạt động, thử nghiệm các khái niệm, hoặc học hỏi điều mới.

## Cách thực hiện:
```TypeScript
// Bước 1: Cài đặt TypeScript ở cấp độ toàn cục (nếu chưa được cài đặt)
npm install -g typescript

// Bước 2: Tạo một thư mục mới cho dự án của bạn
mkdir my-new-project
cd my-new-project

// Bước 3: Khởi tạo một dự án node mới
npm init -y

// Bước 4: Cài đặt TypeScript trong dự án của bạn
npm install typescript --save-dev

// Bước 5: Khởi tạo một dự án TypeScript để tạo tsconfig.json
tsc --init

// Đầu ra mẫu tsconfig.json (với một số trường được lược bỏ cho ngắn gọn)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// Bước 6: Tạo một tập tin TypeScript đơn giản 'hello.ts'
echo 'console.log("Xin chào, TypeScript!");' > hello.ts

// Bước 7: Biên dịch tập tin TypeScript và chạy nó
tsc hello.ts
node hello.js

// Đầu ra mẫu
Xin chào, TypeScript!
```

## Sâu hơn
TypeScript, một siêu tập của JavaScript, được phát triển bởi Microsoft và lần đầu tiên được ra mắt vào tháng 10 năm 2012. Nó thêm kiểu tĩnh vào JavaScript, có thể giúp phát hiện lỗi trước khi chạy và hỗ trợ các tính năng của IDE như điều hướng mã và tái cấu trúc.

Mặc dù quy trình trên sử dụng npm (Node Package Manager), có những cách khác để quản lý các dự án TypeScript, như Yarn hoặc pnpm. Các phương án thay thế để khởi xướng một dự án TypeScript bao gồm việc tạo một dự án bằng bộ khởi đầu hoặc sao chép một bản mẫu từ các kho như GitHub.

`tsconfig.json` rất quan trọng; nó hướng dẫn cách Trình biên dịch TypeScript (tsc) chuyển mã TypeScript của bạn thành JavaScript. Điều chỉnh các tuỳ chọn biên dịch cho phép bạn nhắm đến các phiên bản ECMAScript khác nhau, hệ thống module, và nhiều hơn nữa, tùy chỉnh theo nhu cầu dự án của bạn.

## Tham khảo thêm
- Tài liệu chính thức của TypeScript: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Repo GitHub của TypeScript: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- Sâu hơn về TypeScript: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- Awesome TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:40.295460-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: TypeScript, m\u1ED9t si\xEAu t\u1EADp c\u1EE7\
  a JavaScript, \u0111\u01B0\u1EE3c ph\xE1t tri\u1EC3n b\u1EDFi Microsoft v\xE0 l\u1EA7\
  n \u0111\u1EA7u ti\xEAn \u0111\u01B0\u1EE3c ra m\u1EAFt v\xE0o th\xE1ng 10 n\u0103\
  m 2012. N\xF3 th\xEAm ki\u1EC3u\u2026"
lastmod: '2024-04-05T21:53:37.735110-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, m\u1ED9t si\xEAu t\u1EADp c\u1EE7a JavaScript, \u0111\u01B0\u1EE3\
  c ph\xE1t tri\u1EC3n b\u1EDFi Microsoft v\xE0 l\u1EA7n \u0111\u1EA7u ti\xEAn \u0111\
  \u01B0\u1EE3c ra m\u1EAFt v\xE0o th\xE1ng 10 n\u0103m 2012."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

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

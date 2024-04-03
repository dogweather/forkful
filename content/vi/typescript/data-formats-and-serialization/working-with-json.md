---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:50.944168-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: **Ph\xE2n t\xEDch c\xFA ph\xE1p JSON:**."
lastmod: '2024-03-13T22:44:36.347865-06:00'
model: gpt-4-0125-preview
summary: "**Ph\xE2n t\xEDch c\xFA ph\xE1p JSON:**."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cách thực hiện:
**Phân tích cú pháp JSON:**

```TypeScript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
let user = JSON.parse(jsonString);
console.log(user.name); // John
```

**Chuỗi hóa các đối tượng JavaScript:**

```TypeScript
const userObject = { name: 'Jane', age: 25, city: 'Los Angeles' };
let jsonOutput = JSON.stringify(userObject);
console.log(jsonOutput); // {"name":"Jane","age":25,"city":"Los Angeles"}
```

**Khai báo Kiểu:**

```TypeScript
type User = {
  name: string;
  age: number;
  city: string;
};

const userJson = '{"name":"Jack", "age":28, "city":"Chicago"}';
let user: User = JSON.parse(userJson);
console.log(user.city); // Chicago
```

## Tìm hiểu Sâu hơn
JSON được bắt nguồn từ JavaScript nhưng hiện nay nó không phụ thuộc vào ngôn ngữ lập trình nào; nó đã trở thành lựa chọn hàng đầu cho trao đổi dữ liệu, thay thế XML bởi sự đơn giản của nó. Mặc dù JSON không tự nhiên áp dụng các kiểu dữ liệu (điều mà TypeScript rất chú trọng), TypeScript cho phép bạn định nghĩa các kiểu để đảm bảo cấu trúc JSON của bạn là như bạn mong đợi. Và trong khi JSON là vua cho APIs, đối với các tệp cấu hình, một số người thích dùng YAML, được coi là dễ đọc hơn cho con người. Bên dưới, khi `JSON.parse()` hoặc `JSON.stringify()` được gọi trong TypeScript, thực ra nó đang gọi các hàm JSON của bộ máy JavaScript; vai trò chính của TypeScript là tăng cường các thao tác này với tính an toàn về kiểu.

## Xem thêm
- [JSON.org](https://www.json.org/json-en.html): Tài liệu chính thức về JSON.
- [MDN - Làm việc với JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON): MDN cung cấp nền tảng chung và các trường hợp sử dụng.

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:50.944168-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u nh\u1EB9 \u0111\u01B0\u1EE3c d\xF9ng \u0111\u1EC3 l\u01B0u\
  \ tr\u1EEF v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc, d\u1EC5 ph\xE2n\u2026"
lastmod: '2024-03-13T22:44:36.347865-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u nh\u1EB9 \u0111\u01B0\u1EE3c d\xF9ng \u0111\u1EC3 l\u01B0u\
  \ tr\u1EEF v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc, d\u1EC5 ph\xE2n\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì và Tại sao?

JSON (JavaScript Object Notation) là một định dạng dữ liệu nhẹ được dùng để lưu trữ và truyền tải dữ liệu. Lập trình viên sử dụng nó vì nó dễ đọc, dễ phân tích cú pháp, và được sử dụng rộng rãi trong web APIs và cấu hình.

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

---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:38.322529-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1EA3\
  ng k\u1EBFt h\u1EE3p trong TypeScript l\xE0 tr\u1EF1c ti\u1EBFp. D\u01B0\u1EDBi\
  \ \u0111\xE2y l\xE0 m\u1ED9t h\u01B0\u1EDBng d\u1EABn c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.309355-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1EA3ng k\u1EBFt h\u1EE3p trong\
  \ TypeScript l\xE0 tr\u1EF1c ti\u1EBFp."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Việc tạo và sử dụng mảng kết hợp trong TypeScript là trực tiếp. Dưới đây là một hướng dẫn cơ bản:

```TypeScript
// Khai báo một mảng kết hợp
let user: { [key: string]: string } = {};

// Thêm dữ liệu
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Kết quả:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Lặp qua các cặp khóa-giá trị cũng rất dễ dàng:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Kết quả:

```TypeScript
name: Jane Doe
email: jane@example.com
```

Và nếu bạn đang xử lý một hỗn hợp các loại dữ liệu, hệ thống kiểu của TypeScript rất hữu ích:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Kết quả:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Đi Sâu Hơn
Trong TypeScript, những gì chúng ta gọi là mảng kết hợp thực chất là các đối tượng. Trong lịch sử, trong các ngôn ngữ như PHP, mảng kết hợp là một kiểu cơ bản, nhưng JavaScript (và do đó, TypeScript) sử dụng đối tượng cho mục đích này. Cách tiếp cận này vừa là một điểm mạnh vừa là một hạn chế. Đối tượng cung cấp một cấu trúc động cao cho việc kết hợp chuỗi với giá trị, nhưng chúng không được dự định sử dụng như 'mảng' theo nghĩa truyền thống. Ví dụ, bạn không thể sử dụng trực tiếp các phương thức mảng như `push` hay `pop` trên những đối tượng này.

Trong trường hợp bạn cần các bộ sưu tập có thứ tự của các cặp khóa-giá trị với các hoạt động giống mảng, TypeScript (và JavaScript hiện đại) cung cấp đối tượng `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Trong khi hệ thống kiểu của TypeScript và các tính năng của ES6 như `Map` cung cấp các lựa chọn mạnh mẽ, hiểu biết cách sử dụng các đối tượng như mảng kết hợp là hữu ích cho các tình huống nơi mà các đối tượng theo nghĩa đen hiệu quả hơn hoặc khi làm việc với các cấu trúc dữ liệu JSON. Mọi thứ đều tùy thuộc vào việc chọn công cụ đúng cho công việc.

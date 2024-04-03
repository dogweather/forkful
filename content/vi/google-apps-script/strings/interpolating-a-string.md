---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:17.683306-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, n\u1ED9i suy chu\u1ED7\
  i \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n th\xF4ng qua literals m\u1EABu. \u0110\
  \xE2y l\xE0 c\xE1c literals chu\u1ED7i cho ph\xE9p nh\xFAng bi\u1EC3u th\u1EE9c,\
  \ \u0111\u01B0\u1EE3c bi\u1EC3u\u2026"
lastmod: '2024-03-13T22:44:36.020500-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, n\u1ED9i suy chu\u1ED7i \u0111\u01B0\u1EE3c th\u1EF1\
  c hi\u1EC7n th\xF4ng qua literals m\u1EABu."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Google Apps Script, nội suy chuỗi được thực hiện thông qua literals mẫu. Đây là các literals chuỗi cho phép nhúng biểu thức, được biểu diễn bởi dấu backtick (\`) thay vì dấu ngoặc kép thông thường. Dưới đây là cách bạn có thể sử dụng chúng:

```javascript
// Một ví dụ cơ bản
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Xin chào, ${user}!`); // Đầu ra: Xin chào, Alice!
}

// Sử dụng biểu thức
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Năm cộng mười là ${a + b}.`); // Đầu ra: Năm cộng mười là 15.
}

// Chuỗi nhiều dòng
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Đây là một chuỗi nhiều dòng:
Xin chào tất cả,
Hôm nay chúng ta đang bàn về ${item}.`);
  // Đầu ra:
  // Đây là một chuỗi nhiều dòng:
  // Xin chào tất cả,
  // Hôm nay chúng ta đang bàn về Google Apps Script.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Những ví dụ này minh họa việc sử dụng cơ bản, nhúng biểu thức và tạo ra các chuỗi nhiều dòng với các giá trị được nội suy.

## Đi sâu hơn
Literals mẫu, bao gồm nội suy chuỗi, đã được giới thiệu trong ECMAScript 2015 (ES6) và sau đó được Google Apps Script áp dụng. Trước đó, lập trình viên phải hoàn toàn dựa vào việc nối chuỗi, có thể trở nên khó khăn cho các chuỗi phức tạp hoặc khi tích hợp nhiều giá trị biến.

```javascript
// Cách cũ (trước ES6)
var user = 'Bob';
console.log('Xin chào, ' + user + '!');
```

Mặc dù nội suy chuỗi là một tính năng mạnh mẽ, nhưng quan trọng là phải chú ý đến các bối cảnh mà nó được sử dụng. Ví dụ, việc nhúng trực tiếp đầu vào từ người dùng mà không qua xử lý đúng cách có thể dẫn đến các vấn đề bảo mật, như các cuộc tấn công tiêm nhập. Các nhà phát triển Google Apps Script nên đảm bảo rằng bất kỳ nội dung động nào được nội suy vào chuỗi phải được kiểm tra hoặc xử lý đúng cách.

So với các ngôn ngữ lập trình khác, khái niệm nội suy chuỗi tồn tại rộng rãi, với cú pháp thay đổi. Python sử dụng f-strings hoặc phương pháp `format`, Ruby sử dụng `#{}` trong các chuỗi có dấu ngoặc kép, và nhiều ngôn ngữ hiện đại đã áp dụng các tính năng tương tự do tính dễ đọc và tiện lợi mà chúng mang lại.

Mặc dù Google Apps Script không cung cấp các tính năng nội suy bổ sung ngoài những gì tiêu chuẩn ECMAScript cung cấp, chức năng hiện tại là mạnh mẽ và đủ cho hầu hết các trường hợp sử dụng. Các nhà phát triển đến từ các ngôn ngữ có cơ chế nội suy phức tạp hơn có thể cần điều chỉnh kỳ vọng của mình nhưng chắc chắn sẽ đánh giá cao sự đơn giản và hiệu quả của literals mẫu trong Google Apps Script.

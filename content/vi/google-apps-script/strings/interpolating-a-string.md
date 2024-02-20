---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:17.683306-07:00
description: "Vi\u1EC7c n\u1ED9i suy chu\u1ED7i trong Google Apps Script cho ph\xE9\
  p nh\xFAng \u0111\u1ED9ng c\xE1c bi\u1EC3u th\u1EE9c v\xE0o trong chu\u1ED7i, gi\xFA\
  p t\u1EA1o ra code d\u1EC5 \u0111\u1ECDc v\xE0 b\u1EA3o tr\xEC h\u01A1n. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: 2024-02-19 22:04:55.208556
model: gpt-4-0125-preview
summary: "Vi\u1EC7c n\u1ED9i suy chu\u1ED7i trong Google Apps Script cho ph\xE9p nh\xFA\
  ng \u0111\u1ED9ng c\xE1c bi\u1EC3u th\u1EE9c v\xE0o trong chu\u1ED7i, gi\xFAp t\u1EA1\
  o ra code d\u1EC5 \u0111\u1ECDc v\xE0 b\u1EA3o tr\xEC h\u01A1n. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc nội suy chuỗi trong Google Apps Script cho phép nhúng động các biểu thức vào trong chuỗi, giúp tạo ra code dễ đọc và bảo trì hơn. Lập trình viên sử dụng kỹ thuật này để một cách liền mạch kết hợp các biến và biểu thức vào trong chuỗi mà không cần đến cú pháp nối chuỗi phức tạp.

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

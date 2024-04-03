---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:20.488413-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: JavaScript gi\u1EEF cho n\xF3 \u0111\u01A1\
  n gi\u1EA3n v\u1EDBi thu\u1ED9c t\xEDnh `.length`."
lastmod: '2024-03-13T22:44:37.141424-06:00'
model: gpt-4-0125-preview
summary: "JavaScript gi\u1EEF cho n\xF3 \u0111\u01A1n gi\u1EA3n v\u1EDBi thu\u1ED9\
  c t\xEDnh `.length`."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Cách thực hiện:
JavaScript giữ cho nó đơn giản với thuộc tính `.length`.

```javascript
let greeting = 'Hello, World!';
console.log(greeting.length); // Đầu ra: 13
```

Một chuỗi rỗng bằng không:

```javascript
let empty = '';
console.log(empty.length); // Đầu ra: 0
```

Kể cả khoảng trắng cũng được tính:

```javascript
let spaces = '   ';
console.log(spaces.length); // Đầu ra: 3
```

## Tìm hiểu sâu hơn
Thuộc tính `.length` đã tồn tại từ những ngày đầu của JS. Nó nhanh chóng, vì thực sự nó không phải là một hàm mà là một thuộc tính instance được lưu trữ cùng với đối tượng chuỗi.

Có những phương án khác như việc lặp qua từng ký tự một cách thủ công để đếm chúng, nhưng chúng giống như việc chọn lấy cầu thang thay vì thang máy - chỉ sử dụng khi cần thiết.

JavaScript xử lý chuỗi là không thể thay đổi, có nghĩa là `.length` không thay đổi trừ khi bạn gán một chuỗi mới cho biến. Chiều dài được tính toán khi chuỗi được tạo ra.

Về mặt thực hiện, hãy nhớ về Unicode. Một số ký tự (như emoji hoặc bảng chữ cái của một số ngôn ngữ) có thể được biểu diễn bởi hai hoặc nhiều đơn vị mã trong mã hóa UTF-16 của JavaScript:

```javascript
let smiley = '😊';
console.log(smiley.length); // Đầu ra: 2
```

Dù trông như một ký tự, một số có thể được tính là hai "chiều dài" vì cách chúng được mã hóa. Đây chỉ là điều cần nhớ nếu bạn đang xử lý với các bộ ký tự đa dạng!

## Tham khảo thêm
- [MDN Web Docs - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Unicode và chuỗi trong JavaScript](https://mathiasbynens.be/notes/javascript-unicode)
- [Chuỗi và mã hóa ký tự trong JavaScript](https://flaviocopes.com/javascript-unicode/)

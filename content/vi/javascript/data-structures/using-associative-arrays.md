---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:23.491554-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5\
  ng m\u1EA3ng k\u1EBFt h\u1EE3p (objects) trong JavaScript r\u1EA5t \u0111\u01A1\
  n gi\u1EA3n. B\u1EA1n \u0111\u1ECBnh ngh\u0129a m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3\
  ng v\u1EDBi c\xE1c d\u1EA5u ngo\u1EB7c nh\u1ECDn `{}`, v\xE0 b\xEAn\u2026"
lastmod: '2024-03-13T22:44:37.143920-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1EA3ng k\u1EBFt h\u1EE3p (objects)\
  \ trong JavaScript r\u1EA5t \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Cách thực hiện:
Việc tạo và sử dụng mảng kết hợp (objects) trong JavaScript rất đơn giản. Bạn định nghĩa một đối tượng với các dấu ngoặc nhọn `{}`, và bên trong đó, bạn có thể định nghĩa một tập hợp các cặp khóa-giá trị. Các khóa luôn là chuỗi, và giá trị có thể là bất kỳ thứ gì: chuỗi, số, mảng, thậm chí là các đối tượng khác.

```javascript
// Tạo một mảng kết hợp
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Truy cập các phần tử
console.log(userInfo.name); // Đầu ra: Alex
console.log(userInfo["email"]); // Đầu ra: alex@example.com

// Thêm các phần tử mới
userInfo.job = "Developer";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Đầu ra:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/

// Xóa một phần tử
delete userInfo.age;
console.log(userInfo);
/* Đầu ra:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/
```

Như bạn thấy, việc truy cập, thêm, hoặc xóa các phần tử trong một mảng kết hợp khá trực tiếp và trực quan.

## Sâu hơn nữa
Trong thế giới JavaScript, mặc dù chúng ta thường nghe thuật ngữ "mảng kết hợp," đó kỹ thuật là một tên gọi không chính xác bởi vì JavaScript không có mảng kết hợp thực sự như các ngôn ngữ khác (ví dụ: PHP). Những gì JavaScript có là các đối tượng phục vụ một mục đích tương tự nhưng là một cấu trúc mạnh mẽ và linh hoạt hơn.

Truyền thống, các mảng trong ngôn ngữ lập trình được thiết kế để chứa một tập hợp các mục, được truy cập thông qua chỉ mục số của chúng. Tuy nhiên, khi phát triển phần mềm tiến triển, nhu cầu về các cấu trúc dữ liệu linh hoạt hơn xuất hiện. Mảng kết hợp, hoặc từ điển trong các ngôn ngữ khác, là một phản ứng, cho phép truy cập vào các phần tử thông qua các khóa tùy ý.

Tiếp cận của JavaScript với đối tượng như các kho lưu trữ khóa-giá trị cung cấp một sự pha trộn chức năng. Nó cho phép các thuộc tính (các khóa) được thêm vào, loại bỏ, và tra cứu theo tên. JSON (JavaScript Object Notation) là lời chứng cho tính hữu ích của cấu trúc này, trở thành tiêu chuẩn de facto cho trao đổi dữ liệu trên web.

Mặc dù đối tượng đáp ứng hầu hết nhu cầu về mảng kết hợp, trong các trường hợp mà thứ tự khóa hoặc lặp là quan trọng, đối tượng `Map` được giới thiệu trong ES6 cung cấp một lựa chọn tốt hơn. Một `Map` giữ thứ tự khóa, chấp nhận nhiều loại dữ liệu hơn làm khóa, và bao gồm các phương thức hữu ích cho lặp và truy xuất kích thước. Mặc dù có những lợi ích này, cú pháp đối tượng truyền thống vẫn phổ biến vì sự đơn giản và dễ sử dụng của nó trong nhiều tình huống thông thường.

---
aliases:
- /vi/javascript/using-regular-expressions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:59.178138-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c bi\u1EBFt \u0111\u1EBFn v\u1EDBi t\xEAn g\u1ECDi regex, l\xE0 c\xE1c m\u1EABu\
  \ \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3\
  p k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFA\
  ng\u2026"
lastmod: 2024-02-18 23:08:51.125037
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c bi\u1EBF\
  t \u0111\u1EBFn v\u1EDBi t\xEAn g\u1ECDi regex, l\xE0 c\xE1c m\u1EABu \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p k\xFD\
  \ t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Biểu thức chính quy, thường được biết đến với tên gọi regex, là các mẫu được sử dụng để khớp các kết hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa và thao tác văn bản một cách chính xác và hiệu quả.

## Cách thực hiện:
Dưới đây là cách sử dụng regex trong JavaScript:

```javascript
// Tìm kiếm khớp
const text = "Find the needle in this haystack";
const regex = /needle/;
console.log(text.match(regex));
// Kết quả: ["needle"]

// Thay thế chuỗi
const replacedText = text.replace(regex, "banana");
console.log(replacedText);
// Kết quả: "Find the banana in this haystack"

// Kiểm tra khớp
const exists = regex.test(text);
console.log(exists);
// Kết quả: true

// Sử dụng cờ - 'i' cho khớp không phân biệt chữ hoa chữ thường
const caseInsensitiveRegex = /NEEDLE/i;
console.log(caseInsensitiveRegex.test(text));
// Kết quả: true

// Sử dụng nhóm để trích xuất dữ liệu
const data = "John: 1234, Jane: 5678";
const groupRegex = /(\w+): (\d+)/g;
let match;
while ((match = groupRegex.exec(data)) !== null) {
  console.log(`${match[1]}'s number is ${match[2]}`);
}
// Kết quả: "Số của John là 1234"
// Kết quả: "Số của Jane là 5678"
```

## Tìm hiểu sâu hơn
Regex đã được sử dụng từ những năm 1950 và là một phần của hầu hết các ngôn ngữ lập trình. Mặc dù mạnh mẽ trong việc phân tích văn bản, biểu thức chính quy có thể gây rối; người mới thường thấy chúng khó hiểu. Đối với các nhiệm vụ đơn giản hơn, các phương thức như `String.includes()`, `String.startsWith()`, và `String.endsWith()` có thể được sử dụng như các phương án thay thế. Khi hiệu suất là chìa khóa, hãy nhớ rằng regex có thể chậm - sử dụng chúng một cách khôn ngoan và cân nhắc tối ưu hóa với chuỗi ký tự hoặc vòng lặp cho việc khớp từng ký tự đơn lẻ.

## Xem thêm
- [MDN RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp) – Tài nguyên chuyên sâu về regex JavaScript.
- [RegExr](https://regexr.com/) - Công cụ để học, xây dựng, & kiểm tra regex.
- [RegexOne](https://regexone.com/) - Hướng dẫn tương tác regex dành cho người mới bắt đầu.

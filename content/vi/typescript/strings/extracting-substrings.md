---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:23.120298-07:00
description: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra\
  \ c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i. \u0110i\u1EC1\
  u n\xE0y r\u1EA5t ti\u1EC7n l\u1EE3i cho c\xE1c c\xF4ng vi\u1EC7c nh\u01B0 ph\xE2\
  n t\xEDch d\u1EEF li\u1EC7u, x\xE1c th\u1EF1c \u0111\u1EA7u v\xE0o,\u2026"
lastmod: '2024-03-13T22:44:36.304172-06:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Trong TypeScript, bạn có thể cắt và chọn các chuỗi với các phương thức như `substring()`, `slice()`, và `includes()` của ES6 để tìm kiếm văn bản trong chuỗi.

```TypeScript
let fullString: string = "Hello, TypeScript enthusiasts!";

// Lấy từ ký tự thứ 7 đến 18
let substr: string = fullString.substring(7, 18);
console.log(substr); // In ra: TypeScript

// Tương tự nhưng với slice()
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // In ra: TypeScript

// Kiểm tra xem một chuỗi con có tồn tại không
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // In ra: true
```

## Đi sâu vào vấn đề
Ngày xửa ngày xưa, việc thao tác với chuỗi thường khó khăn hơn—nghĩ đến các hàm xử lý chuỗi của C. Bây giờ, JavaScript và TypeScript cung cấp các phương pháp xử lý Unicode, tôn trọng mã hóa ký tự, và làm việc trực tiếp với các đối tượng chuỗi. `substring()` và `slice()` tương tự nhau nhưng có một điểm khác biệt: `slice()` có thể nhận chỉ số âm, tính ngược từ cuối. `substring()` coi chúng là số không. Trong các tình huống cần xem xét hiệu năng, việc chọn lựa giữa chúng có thể quan trọng, nhưng cho việc sử dụng hàng ngày, điều này khá là tương đương.

```TypeScript
// Sử dụng chỉ số âm với slice
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // In ra: Hello, Type
```

Đối với `includes()`, nó là một lợi ích cho tính dễ đọc hơn so với `indexOf()` truyền thống, giúp ý định của bạn trở nên rõ ràng ngay từ cái nhìn đầu tiên. Không cần phải `if (string.indexOf('một số văn bản') !== -1)` nữa; chỉ cần sử dụng đơn giản `if (string.includes('một số văn bản'))`.

## Xem thêm
- Sổ tay TypeScript về chuỗi, để biết thêm về cách sử dụng kiểu `'string'`: [Chuỗi TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- MDN Web Docs về các phương pháp Chuỗi trong JavaScript, áp dụng cho TypeScript: [MDN Chuỗi](https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String)
- Để hiểu thêm về Unicode và JavaScript (do đó là TypeScript), hãy xem [Hiểu về mã hóa ký tự nội bộ của JavaScript: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)

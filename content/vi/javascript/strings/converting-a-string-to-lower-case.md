---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:14.714545-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong JavaScript, ch\xFAng ta chuy\u1EC3\
  n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDDng b\u1EB1ng ph\u01B0\
  \u01A1ng th\u1EE9c `.toLowerCase()`. N\xF3 \u0111\u01A1n gi\u1EA3n nh\u01B0 sau."
lastmod: '2024-03-13T22:44:37.135656-06:00'
model: gpt-4-0125-preview
summary: "Trong JavaScript, ch\xFAng ta chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7\
  i sang ch\u1EEF th\u01B0\u1EDDng b\u1EB1ng ph\u01B0\u01A1ng th\u1EE9c `.toLowerCase()`."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thực hiện:
Trong JavaScript, chúng ta chuyển đổi một chuỗi sang chữ thường bằng phương thức `.toLowerCase()`. Nó đơn giản như sau:

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hello, world!"
```

Khi sử dụng, mỗi ký tự trong chuỗi gốc (nếu có thể) sẽ được chuyển đổi thành chữ thường:

```javascript
let mixedCase = "jAvAScript ROCKs!";
let lowerCased = mixedCase.toLowerCase();
console.log(lowerCased); // "javascript rocks!"
```

Lưu ý rằng các ký tự không có dạng chữ thường tương đương sẽ giữ nguyên không đổi.

## Đi sâu hơn
Ngày xưa, việc xử lý văn bản cần phải cẩn thận với mã hóa ký tự và chuyển đổi thủ công. Nhưng trong JavaScript hiện đại, `.toLowerCase()` giúp ẩn đi những phức tạp đó. Phía dưới, nó sử dụng bảng ánh xạ Unicode để chuyển đổi ký tự, vì vậy nó hoạt động không chỉ với A-Z.

Có những phương thức khác tồn tại, như:

- `toLocaleLowerCase()`: Điều này tôn trọng ngữ cảnh ngôn ngữ của người dùng, làm cho nó trở nên thiết yếu cho một số ngôn ngữ nơi quy tắc viết thường phụ thuộc vào ngữ cảnh.

- Biểu thức chính quy (Regular expressions): Trước khi có `toLowerCase()`, các nhà phát triển có thể đã sử dụng regex để thủ công thay thế các ký tự chữ hoa.

Về chi tiết, nhớ rằng `.toLowerCase()` không thay đổi chuỗi gốc (chuỗi trong JavaScript là bất biến). Bạn luôn nhận được một chuỗi mới. Nó cũng xử lý tất cả các ký tự được Unicode tiêu chuẩn nhận diện là chữ hoa, điều này nghĩa là bạn được bảo vệ trên các ngôn ngữ và kịch bản khác nhau.

## Xem thêm
- [Tài liệu MDN web về toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Tiêu chuẩn Unicode cho chữ hoa và chữ thường](https://unicode.org/reports/tr21/tr21-5.html)
- [Chữ hoa và chữ thường với Ngữ cảnh: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)

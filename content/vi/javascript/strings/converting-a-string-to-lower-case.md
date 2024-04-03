---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:14.714545-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\
  \u1EDDng ngh\u0129a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c k\xFD\
  \ t\u1EF1 trong \u0111\xF3 th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng c\u1EE7\
  a ch\xFAng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3\u2026"
lastmod: '2024-03-13T22:44:37.135656-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\
  \u1EDDng ngh\u0129a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c k\xFD\
  \ t\u1EF1 trong \u0111\xF3 th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng c\u1EE7\
  a ch\xFAng."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Gì và Tại sao?
Chuyển đổi một chuỗi thành chữ thường nghĩa là biến đổi tất cả các ký tự trong đó thành dạng chữ thường của chúng. Lập trình viên thực hiện việc này để đảm bảo tính nhất quán, đặc biệt là cho các so sánh không phân biệt chữ hoa chữ thường, như khi chuẩn hóa đầu vào của người dùng hoặc tìm kiếm trong nội dung văn bản.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:44.365575-07:00
description: "L\xE0m th\u1EBF n\xE0o: JavaScript kh\xF4ng c\xF3 ph\u01B0\u01A1ng th\u1EE9\
  c t\xEDch h\u1EE3p s\u1EB5n \u0111\u1EC3 vi\u1EBFt hoa, nh\u01B0ng d\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 m\u1ED9t h\xE0m \u0111\u01A1n gi\u1EA3n th\u1EF1c hi\u1EC7n\
  \ \u0111i\u1EC1u \u0111\xF3."
lastmod: '2024-03-13T22:44:37.130906-06:00'
model: gpt-4-0125-preview
summary: "JavaScript kh\xF4ng c\xF3 ph\u01B0\u01A1ng th\u1EE9c t\xEDch h\u1EE3p s\u1EB5\
  n \u0111\u1EC3 vi\u1EBFt hoa, nh\u01B0ng d\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t h\xE0m \u0111\u01A1n gi\u1EA3n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u \u0111\xF3."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
JavaScript không có phương thức tích hợp sẵn để viết hoa, nhưng dưới đây là một hàm đơn giản thực hiện điều đó:

```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('hello')); // Kết quả: Hello
```

Đối với nhiều từ:

```javascript
function capitalizeWords(str) {
    return str.split(' ').map(capitalizeFirstLetter).join(' ');
}

console.log(capitalizeWords('hello world!')); // Kết quả: Hello World!
```

## Tìm hiểu sâu hơn
Việc viết hoa chuỗi không luôn có các hàm tích hợp sẵn trong các ngôn ngữ, thường liên quan đến việc thao tác ASCII thủ công. Ngày nay, hầu hết các ngôn ngữ lập trình đều cung cấp các phương thức để thao tác chuỗi, nhưng JavaScript yêu cầu một cách tiếp cận tự làm nhiều hơn.

### Các phương án thay thế:
Bạn có thể sử dụng CSS để viết hoa văn bản cho các trang web (`text-transform: capitalize;`), hoặc các thư viện như Lodash có các hàm viết hoa. Nhưng việc thực hiện bằng JavaScript thuần túy, như đã trình bày ở trên, không phụ thuộc vào bất kỳ thứ gì khác.

### Chi tiết thực hiện:
`charAt(0)` lấy ký tự đầu tiên. `toUpperCase()` chuyển nó thành chữ in hoa. Kết hợp nó với phần còn lại của chuỗi `slice(1)` để bạn có được một chuỗi được viết hoa. Phương pháp này hoạt động tốt giả sử đầu vào là một chuỗi và không bắt đầu bằng một khoảng trắng.

## Xem thêm:
- CSS text-transform của MDN để viết hoa: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
- Tài liệu về phương thức capitalize của Lodash: https://lodash.com/docs/4.17.15#capitalize
- JavaScript String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:11.657481-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1ED1i t\u01B0\u1EE3ng `Date` trong\
  \ JavaScript l\xE0 c\xF4ng c\u1EE5 b\u1EA1n c\u1EA7n \u0111\u1EC3 l\xE0m \"gymnastics\"\
  \ v\u1EDBi ng\xE0y th\xE1ng. H\xE3y th\u1EED v\u1EDBi m\u1ED9t s\u1ED1 v\xED d\u1EE5\
  ."
lastmod: '2024-03-13T22:44:37.170951-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ED1i t\u01B0\u1EE3ng `Date` trong JavaScript l\xE0 c\xF4ng c\u1EE5\
  \ b\u1EA1n c\u1EA7n \u0111\u1EC3 l\xE0m \"gymnastics\" v\u1EDBi ng\xE0y th\xE1ng."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cách thực hiện:
Đối tượng `Date` trong JavaScript là công cụ bạn cần để làm "gymnastics" với ngày tháng. Hãy thử với một số ví dụ:

```javascript
// Ngày hôm nay
let today = new Date();
console.log(today); // Xuất ra ngày và thời gian hiện tại

// Tính toán một ngày cách 7 ngày trong tương lai
let nextWeek = new Date();
nextWeek.setDate(today.getDate() + 7);
console.log(nextWeek); // Xuất ra ngày cùng thời điểm, 7 ngày về sau

// Tính toán một ngày cách 30 ngày trong quá khứ
let lastMonth = new Date();
lastMonth.setDate(today.getDate() - 30);
console.log(lastMonth); // Xuất ra ngày cùng thời điểm, 30 ngày trước

// Đặt một ngày 1 năm trong tương lai
let nextYear = new Date();
nextYear.setFullYear(today.getFullYear() + 1);
console.log(nextYear); // Xuất ra ngày cùng thời điểm vào năm sau
```
Kết quả xuất ra phụ thuộc vào thời điểm bạn chạy mã này, vì `today` là ngày-giờ hiện tại của bạn.

## Đi sâu hơn
Trước khi JavaScript có chức năng tính toán ngày tháng được tích hợp sẵn, các lập trình viên phải tính toán ngày một cách thủ công, tính đến sự khác biệt về độ dài các tháng, năm nhuận, và múi giờ - một việc thực sự phiền phức! Với `Date`, phần lớn những phiền toái này biến mất.

Các lựa chọn thay thế cho đối tượng `Date` bản địa bao gồm các thư viện như `moment.js` và `date-fns`, cung cấp cú pháp phong phú hơn và giải quyết những vấn đề như lỗi giờ mùa hè.

Khi tính toán ngày, nhớ rằng: `Date` đếm các tháng từ 0 (Tháng Một) đến 11 (Tháng Mười Hai), không phải từ 1-12. Và đừng quên năm nhuận khi làm việc với ngày của Tháng Hai.

## Xem thêm
- Tài liệu MDN Web về Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/

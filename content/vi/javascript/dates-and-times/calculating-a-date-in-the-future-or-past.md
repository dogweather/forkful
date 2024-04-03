---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:11.657481-07:00
description: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 c\xF3 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y c\xE1ch m\u1ED9\
  t s\u1ED1 ng\xE0y, tu\u1EA7n, th\xE1ng, ho\u1EB7c n\u0103m t\u1EEB m\u1ED9t \u0111\
  i\u1EC3m nh\u1EA5t \u0111\u1ECBnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:37.170951-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9 c\xF3 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y c\xE1ch m\u1ED9t s\u1ED1\
  \ ng\xE0y, tu\u1EA7n, th\xE1ng, ho\u1EB7c n\u0103m t\u1EEB m\u1ED9t \u0111i\u1EC3\
  m nh\u1EA5t \u0111\u1ECBnh."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cái gì & Tại sao?
Tính toán một ngày trong tương lai hoặc quá khứ có nghĩa là tìm một ngày cách một số ngày, tuần, tháng, hoặc năm từ một điểm nhất định. Các lập trình viên thường cần điều này cho các nhiệm vụ như thiết lập ngày hết hạn, nhắc nhở, hoặc lập lịch cho các sự kiện.

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

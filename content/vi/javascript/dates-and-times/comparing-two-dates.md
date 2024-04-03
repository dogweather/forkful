---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:36.658702-07:00
description: "C\xE1ch l\xE0m: C\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng `Date` trong JavaScript\
  \ r\u1EA5t ti\u1EC7n l\u1EE3i. Khi b\u1EA1n so s\xE1nh ch\xFAng, ch\xFAng \u0111\
  \u01B0\u1EE3c chuy\u1EC3n \u0111\u1ED5i th\xE0nh milli gi\xE2y k\u1EC3 t\u1EEB ng\xE0\
  y 1 th\xE1ng 1 n\u0103m 1970,\u2026"
lastmod: '2024-03-13T22:44:37.169685-06:00'
model: gpt-4-0125-preview
summary: "C\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng `Date` trong JavaScript r\u1EA5t ti\u1EC7\
  n l\u1EE3i."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách làm:
Các đối tượng `Date` trong JavaScript rất tiện lợi. Khi bạn so sánh chúng, chúng được chuyển đổi thành milli giây kể từ ngày 1 tháng 1 năm 1970, UTC.

```javascript
let date1 = new Date('2021-07-24');
let date2 = new Date('2021-07-25');

console.log(date1 < date2); // true
console.log(date1 > date2); // false
console.log(date1.getTime() === date2.getTime()); // false
```

Kết quả mẫu:

```
true
false
false
```

## Tìm hiểu sâu hơn
Bên trong, các đối tượng `Date` chỉ là milli giây. Trong lịch sử, lập trình viên phải quản lý thao tác với ngày một cách thủ công, tính toán thời gian đã trôi qua từ một điểm dữ liệu, thường gặp rủi ro về lỗi. So sánh các đối tượng `Date` làm cho cuộc sống dễ dàng hơn, mặc dù vẫn không hoàn toàn tránh khỏi lỗi, đặc biệt là với các múi giờ và giờ tiết kiệm ánh sáng.

Có phương pháp thay thế? Chắc chắn. Các thư viện như `moment.js` hoặc `date-fns` giúp xử lý các kịch bản phức tạp và cung cấp thêm tiện ích cho việc thao tác với ngày.

Về việc triển khai, điều quan trọng cần nhớ là so sánh trực tiếp các đối tượng `Date` (với `==`) so sánh tham chiếu, không phải giá trị. Sử dụng `getTime()` cho một so sánh giá trị chính xác. Và hãy cẩn thận với các múi giờ khi phân tích cú pháp cho ngày; rất dễ bị nhầm lẫn nếu bạn không cẩn thận.

## Xem thêm
- Tài liệu MDN về Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Thư viện Moment.js: https://momentjs.com/
- Thư viện date-fns: https://date-fns.org/

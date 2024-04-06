---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:14.804051-07:00
description: "C\xE1ch th\u1EE9c: Ng\xE0y x\u01B0a, \u0111\u1ED1i t\u01B0\u1EE3ng `Date`\
  \ c\u1EE7a JavaScript \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng \u0111\u1EC3 x\u1EED\
  \ l\xFD ng\xE0y v\xE0 gi\u1EDD. M\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng `Date` \u0111\
  \u1EA1i di\u1EC7n cho m\u1ED9t th\u1EDDi \u0111i\u1EC3m duy nh\u1EA5t trong\u2026"
lastmod: '2024-04-05T21:53:38.512599-06:00'
model: gpt-4-0125-preview
summary: "Ng\xE0y x\u01B0a, \u0111\u1ED1i t\u01B0\u1EE3ng `Date` c\u1EE7a JavaScript\
  \ \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng \u0111\u1EC3 x\u1EED l\xFD ng\xE0y v\xE0\
  \ gi\u1EDD."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cách thức:
```javascript
const now = new Date();
console.log(now.toString());  // Ví dụ kết quả: Wed Apr 05 2023 20:46:28 GMT-0400 (Eastern Daylight Time)

console.log(now.toISOString());  // Ví dụ kết quả: 2023-04-05T20:46:28.000Z
```

## Tìm hiểu sâu hơn
Ngày xưa, đối tượng `Date` của JavaScript được xây dựng để xử lý ngày và giờ. Một đối tượng `Date` đại diện cho một thời điểm duy nhất trong thời gian, đến từng mili giây.

**Các phương án khác:**
- Các thư viện như Moment.js (mặc dù giờ đây nó được coi là lỗi thời), date-fns, hoặc Luxon có thể cung cấp nhiều tính năng hơn.
- Với Node.js, bạn có thể sử dụng các mô-đun tích hợp sẵn cho thời gian, nhưng trong hầu hết các trường hợp, đối tượng `Date` gốc vẫn hoạt động tốt.

**Chi tiết triển khai:**
- `Date` có thể được chuyển thành một chuỗi hoặc một định dạng cụ thể bằng các phương thức như `.toString(), .toISOString()`.
- Những điểm bất cập về múi giờ là vấn đề thường gặp. Lưu ý, `.toISOString()` trả về thời gian UTC.
- JavaScript đếm thời gian dưới dạng mili giây kể từ Epoch Unix (01-01-1970, 00:00:00 UTC). Bạn có thể lấy điều này với `Date.now()`.

## Xem thêm
- [MDN Web Docs về Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Bạn Không Cần Moment.js](https://you-dont-need.github.io/You-Dont-Need-Momentjs/)
- [Tài liệu Luxon](https://moment.github.io/luxon/)

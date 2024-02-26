---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:36.658702-07:00
description: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 ki\u1EC3m tra xem ch\xFA\
  ng c\xF3 gi\u1ED1ng nhau kh\xF4ng ho\u1EB7c x\xE1c \u0111\u1ECBnh c\xE1i n\xE0o\
  \ \u0111\u1EBFn tr\u01B0\u1EDBc ho\u1EB7c sau. L\u1EADp tr\xECnh vi\xEAn th\u01B0\
  \u1EDDng c\u1EA7n \u0111i\u1EC1u n\xE0y cho c\xE1c\u2026"
lastmod: '2024-02-25T18:49:35.516448-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 ki\u1EC3m tra xem ch\xFAng\
  \ c\xF3 gi\u1ED1ng nhau kh\xF4ng ho\u1EB7c x\xE1c \u0111\u1ECBnh c\xE1i n\xE0o \u0111\
  \u1EBFn tr\u01B0\u1EDBc ho\u1EB7c sau. L\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDD\
  ng c\u1EA7n \u0111i\u1EC1u n\xE0y cho c\xE1c\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày có nghĩa là kiểm tra xem chúng có giống nhau không hoặc xác định cái nào đến trước hoặc sau. Lập trình viên thường cần điều này cho các hạn chót, lên lịch sự kiện, hoặc chỉ đơn giản là theo dõi thời gian.

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

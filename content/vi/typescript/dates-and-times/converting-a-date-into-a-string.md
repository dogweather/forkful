---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:49.527155-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y ngh\u0129 v\u1EC1 \u0111\u1ECBnh d\u1EA1\
  ng chu\u1ED7i ng\xE0y nh\u01B0 l\xE0 h\u1ED9 chi\u1EBFu c\u1EE7a n\xF3, cho ph\xE9\
  p n\xF3 di chuy\u1EC3n qua c\xE1c ranh gi\u1EDBi h\u1EC7 th\u1ED1ng - t\u1EEB c\u01A1\
  \ s\u1EDF d\u1EEF li\u1EC7u \u0111\u1EBFn trang web.\u2026"
lastmod: '2024-04-05T21:53:37.747958-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y ngh\u0129 v\u1EC1 \u0111\u1ECBnh d\u1EA1ng chu\u1ED7i ng\xE0y nh\u01B0\
  \ l\xE0 h\u1ED9 chi\u1EBFu c\u1EE7a n\xF3, cho ph\xE9p n\xF3 di chuy\u1EC3n qua\
  \ c\xE1c ranh gi\u1EDBi h\u1EC7 th\u1ED1ng - t\u1EEB c\u01A1 s\u1EDF d\u1EEF li\u1EC7\
  u \u0111\u1EBFn trang web."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Làm thế nào:
```TypeScript
// Chuyển đổi đơn giản sử dụng toLocaleString()
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // "4/3/2023, 1:15:30 PM" (sẽ thay đổi tùy theo ngôn ngữ)

// Định dạng ISO sử dụng toISOString()
let isoString = date.toISOString();
console.log(isoString); // "2023-04-03T13:15:30.000Z"

// Định dạng tùy chỉnh sử dụng toLocaleDateString()
let customString = date.toLocaleDateString('en-US', {
  year: 'numeric',
  month: 'long',
  day: 'numeric',
});
console.log(customString); // "April 3, 2023"
```

## Tìm hiểu sâu hơn
Hãy nghĩ về định dạng chuỗi ngày như là hộ chiếu của nó, cho phép nó di chuyển qua các ranh giới hệ thống - từ cơ sở dữ liệu đến trang web. Lịch sử, chúng ta đã gặp vấn đề với các định dạng ngày không nhất quán, đó là lý do tại sao các tiêu chuẩn như ISO 8601 được giới thiệu. Điều này làm cho việc trao đổi ngày trở nên đơn giản trên toàn thế giới.

Các phương pháp thay thế cho các phương pháp được xây dựng sẵn? Thư viện! Moment.js đã là lựa chọn hàng đầu trong nhiều năm, nhưng ngày nay date-fns hoặc Luxon được ưa chuộng hơn - chúng nhẹ hơn và có tính mô-đun cao hơn.

Bản chất của những chuyển đổi này nằm ở các phương pháp được sử dụng. `toLocaleString()` dựa vào ngôn ngữ của người dùng, làm cho nó hoàn hảo để hiển thị cho người dùng. `toISOString()`, tuy nhiên, giữ trung thành với định dạng ISO 8601, điều này rất tuyệt vời cho việc tuần tự hóa và lưu trữ ngày trong một định dạng tiêu chuẩn. Và `toLocaleDateString()` cho bạn kiểm soát về hình thức, phục vụ cho nhu cầu phong cách cụ thể.

## Xem thêm
- [Đối tượng Ngày - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tài liệu date-fns](https://date-fns.org/docs/Getting-Started)
- [Tài liệu Luxon](https://moment.github.io/luxon/)
- [Định dạng ngày và thời gian ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:14.804051-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong JavaScript l\xE0 vi\u1EC7\
  c thu th\u1EADp ng\xE0y v\xE0 th\u1EDDi gian c\u1EE7a ng\xE0y hi\u1EC7n t\u1EA1\
  i. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1\
  c m\u1EE5c \u0111\xEDch nh\u01B0 d\u1EA5u th\u1EDDi\u2026"
lastmod: 2024-02-19 22:04:56.394029
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong JavaScript l\xE0 vi\u1EC7c thu\
  \ th\u1EADp ng\xE0y v\xE0 th\u1EDDi gian c\u1EE7a ng\xE0y hi\u1EC7n t\u1EA1i. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1c m\u1EE5\
  c \u0111\xEDch nh\u01B0 d\u1EA5u th\u1EDDi\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Lấy ngày hiện tại trong JavaScript là việc thu thập ngày và thời gian của ngày hiện tại. Lập trình viên thực hiện điều này cho các mục đích như dấu thời gian, lịch trình, và logic dựa trên thời gian.

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

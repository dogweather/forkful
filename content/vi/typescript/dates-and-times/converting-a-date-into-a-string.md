---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases:
- /vi/typescript/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:49.527155-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi một ngày thành chuỗi thay đổi đối tượng ngày thành định dạng văn bản. Lập trình viên làm điều này để dễ đọc hơn, lưu trữ hoặc hiển thị ngày cho người dùng.

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

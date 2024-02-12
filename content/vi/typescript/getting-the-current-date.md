---
title:                "Lấy ngày hiện tại"
aliases:
- vi/typescript/getting-the-current-date.md
date:                  2024-01-28T22:01:37.810866-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Lấy ngày hiện tại trong mã lệnh của bạn có nghĩa là nắm bắt khoảnh khắc hiện tại đến từng ngày. Lập trình viên làm điều này để đánh dấu thời gian cho các sự kiện, xử lý lịch trình, và theo dõi thời lượng hoặc các khoảng thời gian.

## Làm thế nào:
Dưới đây là cách bạn lấy ngày hiện tại trong TypeScript:

```typescript
// Lấy ngày và giờ hiện tại
const now = new Date();

// In ra console
console.log(now);
```

Kết quả mẫu có thể trông như thế này:

```
2023-04-01T12:34:56.789Z
```

Nhưng nếu bạn chỉ muốn ngày mà không cần thời gian:

```typescript
const today = new Date().toISOString().split('T')[0];

console.log(today);
```

Và bạn sẽ nhận được:

```
2023-04-01
```

## Tìm hiểu sâu hơn
Đối tượng `Date` của JavaScript là cái bạn đang làm việc với nó trong TypeScript cho ngày và giờ. Nó đã xuất hiện từ những ngày đầu, được tạo ra như một phần của ECMAScript 1 năm 1997. Các phương án thay thế cho `Date` gốc bao gồm thư viện như `moment.js` hoặc `date-fns`, cung cấp nhiều tính năng hơn và việc phân tích cú pháp tốt hơn.

Bên trong, `new Date()` cho bạn số lượng mili giây kể từ thời điểm Unix Epoch (1 tháng 1 năm 1970). Đó là cách máy tính theo dõi thời gian. Các múi giờ có thể là một vấn đề phức tạp, đặc biệt là khi bạn cần hiển thị ngày cho người dùng trên toàn thế giới. Theo mặc định, `new Date()` sẽ sử dụng thời gian địa phương của hệ thống. Phương thức `toISOString()` chuyển đổi ngày sang Thời gian Phối hợp Quốc tế (UTC) và định dạng nó thành một chuỗi ISO.

## Xem thêm
- MDN Web Docs về `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Xử lý múi giờ trong JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString

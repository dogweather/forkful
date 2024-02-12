---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases:
- /vi/javascript/converting-a-date-into-a-string/
date:                  2024-01-28T21:57:32.458591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do và vì sao?
Chuyển đổi từ ngày sang chuỗi biến một đối tượng Date thành định dạng văn bản dễ đọc, bởi vì con người thích "Ngày 1 Tháng 4, 2023" hơn là những dấu thời gian khó hiểu. Các lập trình viên thực hiện điều này để làm rõ trong giao diện người dùng và để định dạng ngày tháng cho việc lưu trữ hoặc chuyển giao mạng.

## Cách thực hiện:
JavaScript có những phương thức được xây dựng sẵn để chuyển đổi ngày sang chuỗi. Dưới đây là cách sử dụng chúng:

```javascript
const now = new Date();

// toLocaleString() - định dạng địa phương
console.log(now.toLocaleString()); // '4/1/2023, 12:00:00 PM'

// toString() - định dạng tiêu chuẩn
console.log(now.toString()); // 'Sat Apr 01 2023 12:00:00 GMT+0100 (Giờ Chuẩn Trung Âu)'

// toISOString() - định dạng ISO (tuyệt vời cho cơ sở dữ liệu/mạng)
console.log(now.toISOString()); // '2023-04-01T11:00:00.000Z'
```

## Tìm hiểu sâu hơn
Trong quá khứ, việc chuyển đổi ngày sang chuỗi là một mớ hỗn độn—không có tiêu chuẩn, chỉ là một loạt các hàm tự tạo. May mắn thay, ECMAScript đã bước vào, chuẩn hóa đối tượng Date trong ES5 và thêm `toISOString()` tiện lợi trong ES5.1.

Các phương thức thay thế cho các phương thức bản địa bao gồm các thư viện như `moment.js` và `date-fns`, cung cấp nhiều kiểm soát hơn và xử lý múi giờ, nhưng chúng sẽ làm tăng kích thước dự án của bạn.

Bên dưới, khi bạn gọi một phương thức chuyển đổi ngày sang chuỗi, JavaScript tương tác với cài đặt địa phương và thông tin múi giờ của hệ thống để tạo ra đầu ra chuỗi. Trái lại, `toISOString()` luôn trả về thời gian UTC (chữ 'Z' có nghĩa là 'Zulu time' hoặc không chênh lệch so với UTC).

## Xem thêm
- [MDN Web Docs – Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Định dạng ngày và giờ ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [date-fns](https://date-fns.org/)

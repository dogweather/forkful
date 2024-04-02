---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:32.458591-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i t\u1EEB ng\xE0y sang chu\u1ED7i bi\u1EBFn\
  \ m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng Date th\xE0nh \u0111\u1ECBnh d\u1EA1ng\
  \ v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc, b\u1EDFi v\xEC con ng\u01B0\u1EDDi th\xED\
  ch \"Ng\xE0y 1 Th\xE1ng 4, 2023\" h\u01A1n l\xE0 nh\u1EEFng d\u1EA5u th\u1EDDi\u2026"
lastmod: '2024-03-13T22:44:37.168418-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i t\u1EEB ng\xE0y sang chu\u1ED7i bi\u1EBFn m\u1ED9\
  t \u0111\u1ED1i t\u01B0\u1EE3ng Date th\xE0nh \u0111\u1ECBnh d\u1EA1ng v\u0103n\
  \ b\u1EA3n d\u1EC5 \u0111\u1ECDc, b\u1EDFi v\xEC con ng\u01B0\u1EDDi th\xEDch \"\
  Ng\xE0y 1 Th\xE1ng 4, 2023\" h\u01A1n l\xE0 nh\u1EEFng d\u1EA5u th\u1EDDi\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

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

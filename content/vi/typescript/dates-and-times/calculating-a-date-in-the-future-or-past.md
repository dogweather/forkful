---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:55.962154-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong l\u1ECBch s\u1EED, qu\u1EA3n l\xFD\
  \ ng\xE0y trong JavaScript\u2014v\xE0 theo \u0111\xF3 l\xE0 TypeScript\u2014\u0111\
  \xE3 g\u1EB7p ph\u1EA3i kh\xF3 kh\u0103n do \u0111i\u1EC1u k\u1EF3 l\u1EA1 c\u1EE7\
  a \u0111\u1ED1i t\u01B0\u1EE3ng Date v\xE0 m\xFAi gi\u1EDD.\u2026"
lastmod: '2024-04-05T22:50:50.681812-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1ECBch s\u1EED, qu\u1EA3n l\xFD ng\xE0y trong JavaScript\u2014\
  v\xE0 theo \u0111\xF3 l\xE0 TypeScript\u2014\u0111\xE3 g\u1EB7p ph\u1EA3i kh\xF3\
  \ kh\u0103n do \u0111i\u1EC1u k\u1EF3 l\u1EA1 c\u1EE7a \u0111\u1ED1i t\u01B0\u1EE3\
  ng Date v\xE0 m\xFAi gi\u1EDD."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cách thực hiện:
```TypeScript
// Lấy ngày hiện tại
const today: Date = new Date();

// Tính toán 10 ngày trong tương lai
const tenDaysLater: Date = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(`Mười ngày từ bây giờ: ${tenDaysLater.toDateString()}`);

// Tính toán 10 ngày trong quá khứ
const tenDaysBefore: Date = new Date(today.getTime() - (10 * 24 * 60 * 60 * 1000));
console.log(`Mười ngày trước là: ${tenDaysBefore.toDateString()}`);
```
Kết quả mẫu:
```
Mười ngày từ bây giờ: Chủ nhật, 23 Tháng 4 2023
Mười ngày trước là: Thứ tư, 3 Tháng 4 2023
```

## Nghiên cứu sâu
Trong lịch sử, quản lý ngày trong JavaScript—và theo đó là TypeScript—đã gặp phải khó khăn do điều kỳ lạ của đối tượng Date và múi giờ. Các thư viện bổ trợ như Moment.js và date-fns đã đưa ra các trừu tượng hóa để xử lý sự phức tạp này. Với ES6, hỗ trợ tốt hơn cho quốc tế hóa đã xuất hiện thông qua API `Intl`, mà TypeScript cũng có thể sử dụng.

Khi tính toán ngày, hãy theo dõi sự thay đổi của giờ mùa hè và giây nhuận. Những thay đổi này có thể làm rối tung các tính toán đơn giản như thêm 24 giờ vào một ngày. Ngoài ra, luôn cần xem xét địa điểm và múi giờ của người dùng khi hiển thị các ngày được tính toán.

Đối với tính tương thích và linh hoạt rộng rãi, bạn có thể chọn các thư viện như `date-fns` hoặc `Luxon`, chúng là modular và có thể tuyệt vời cho các nhiệm vụ phức tạp. Chẳng hạn, với `date-fns`, bạn có thể dễ dàng thêm ngày:

```TypeScript
import { addDays } from 'date-fns';

const result = addDays(new Date(2023, 3, 13), 10); // 13 Tháng 4, 2023 + 10 ngày
console.log(result.toDateString());
```

Chúng cũng xử lý các trường hợp ngoại lệ và vấn đề múi giờ, lấy đi nhiều sự đau đớn khỏi phép tính ngày.

## Xem Thêm
- [Tài liệu tham khảo MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Thư viện date-fns](https://date-fns.org/)
- [Tài liệu Luxon](https://moment.github.io/luxon/#/)
- [Tài liệu chính thức của TypeScript](https://www.typescriptlang.org/docs/)

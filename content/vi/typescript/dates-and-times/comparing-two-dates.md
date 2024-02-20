---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:41.171185-07:00
description: "So s\xE1nh hai ng\xE0y bao g\u1ED3m vi\u1EC7c x\xE1c \u0111\u1ECBnh\
  \ m\u1ED1i quan h\u1EC7 v\u1EC1 th\u1EDDi gian c\u1EE7a ch\xFAng\u2014ch\xFAng c\xF3\
  \ gi\u1ED1ng nhau kh\xF4ng, c\xE1i n\xE0o s\u1EDBm h\u01A1n, hay c\xF3 th\u1EC3\
  \ l\xE0 mu\u1ED9n h\u01A1n? L\u1EADp tr\xECnh\u2026"
lastmod: 2024-02-19 22:04:55.486357
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y bao g\u1ED3m vi\u1EC7c x\xE1c \u0111\u1ECBnh m\u1ED1\
  i quan h\u1EC7 v\u1EC1 th\u1EDDi gian c\u1EE7a ch\xFAng\u2014ch\xFAng c\xF3 gi\u1ED1\
  ng nhau kh\xF4ng, c\xE1i n\xE0o s\u1EDBm h\u01A1n, hay c\xF3 th\u1EC3 l\xE0 mu\u1ED9\
  n h\u01A1n? L\u1EADp tr\xECnh\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày bao gồm việc xác định mối quan hệ về thời gian của chúng—chúng có giống nhau không, cái nào sớm hơn, hay có thể là muộn hơn? Lập trình viên làm việc này để lên lịch cho các sự kiện, sắp xếp các dòng thời gian và kiểm tra khoảng thời gian.

## Làm thế nào:

Hãy so sánh một số ngày:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// date1 có trước date2 không?
console.log(date1 < date2); // true

// date1 có giống date2 không?
console.log(date1.getTime() === date2.getTime()); // false

// Cách nhau bao nhiêu ngày?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // 1
```

Kết quả mẫu:

```
true
false
1
```

## Ôn lại

Trước đây, ngày tháng là một đống hỗn độn về định dạng và các tính toán lộn xộn. Với JavaScript (và TypeScript mở rộng), đối tượng `Date` đã đơn giản hóa mọi thứ, tiêu chuẩn hóa cách chúng ta xử lý thời gian.

Có phương án khác không? Chắc chắn. Các thư viện như `moment.js` hay `date-fns` tăng cường khả năng xử lý ngày tháng với chức năng bổ sung. Nhưng đối với các so sánh cơ bản? Sự đơn giản của Native Date thường làm tốt công việc.

Bên trong, `Date.getTime()` lấy số milliseconds từ kỷ nguyên (ngày 1 tháng 1 năm 1970). So sánh các giá trị này loại bỏ đi những rắc rối về múi giờ và giây nhuận, đưa mọi thứ về con số.

## Xem thêm

- [Mozilla Developer Network Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) để biết chi tiết về đối tượng Date.
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs) cho những lúc bạn có thể, hoặc có thể không, cần một thư viện.
- [Tài liệu Chính thức của TypeScript](https://www.typescriptlang.org/docs/) để biết thêm về sức mạnh và những hạn chế của TypeScript.

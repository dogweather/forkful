---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:41.171185-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc \u0111\xE2y, ng\xE0y th\xE1\
  ng l\xE0 m\u1ED9t \u0111\u1ED1ng h\u1ED7n \u0111\u1ED9n v\u1EC1 \u0111\u1ECBnh d\u1EA1\
  ng v\xE0 c\xE1c t\xEDnh to\xE1n l\u1ED9n x\u1ED9n. V\u1EDBi JavaScript (v\xE0 TypeScript\
  \ m\u1EDF r\u1ED9ng), \u0111\u1ED1i t\u01B0\u1EE3ng `Date` \u0111\xE3\u2026"
lastmod: '2024-04-05T22:50:50.679630-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc \u0111\xE2y, ng\xE0y th\xE1ng l\xE0 m\u1ED9t \u0111\u1ED1\
  ng h\u1ED7n \u0111\u1ED9n v\u1EC1 \u0111\u1ECBnh d\u1EA1ng v\xE0 c\xE1c t\xEDnh\
  \ to\xE1n l\u1ED9n x\u1ED9n."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

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

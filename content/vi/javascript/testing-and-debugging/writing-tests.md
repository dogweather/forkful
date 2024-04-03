---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:10.524242-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng\
  \ m\u1ED9t h\xE0m \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 c\u1ED9ng hai s\u1ED1 trong\
  \ JavaScript."
lastmod: '2024-03-13T22:44:37.157983-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng m\u1ED9t h\xE0m \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 c\u1ED9ng hai s\u1ED1 trong JavaScript."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cách thực hiện:
Hãy tưởng tượng một hàm đơn giản để cộng hai số trong JavaScript:

```javascript
function add(a, b) {
  return a + b;
}
```

Để kiểm thử điều này, bạn có thể sử dụng một framework kiểm thử như Jest. Dưới đây là cách bạn viết một bài test cơ bản:

```javascript
const add = require('./add'); // giả sử hàm add nằm trong 'add.js'

test('cộng 1 + 2 bằng 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Chạy các tests, và Jest sẽ thông báo cho bạn nếu hàm `add` đã vượt qua:

```plaintext
PASS  ./add.test.js
✓ cộng 1 + 2 bằng 3 (5ms)
```

## Sâu hơn
Trong lịch sử, kiểm thử thủ công, tẻ nhạt và dễ sai sót. Sự ra đời của kiểm thử tự động vào cuối thế kỷ 20 đã cải thiện tình hình này, với TDD (Phát Triển Hướng Kiểm Thử) trở thành một phương pháp chính. Các lựa chọn khác cho Jest bao gồm Mocha, Jasmine và QUnit, cùng nhiều công cụ khác. Chi tiết thực hiện chính trong việc viết tests là phát biểu kiểm định: một phát biểu kiểm tra xem điều gì đó có đúng không. Nếu các kiểm định vượt qua, bài test của bạn cũng vượt qua.

## Xem thêm
- Jest: https://jestjs.io/
- Phát Triển Hướng Kiểm Thử: https://en.wikipedia.org/wiki/Test-driven_development
- Mocha: https://mochajs.org/
- Jasmine: https://jasmine.github.io/
- QUnit: https://qunitjs.com/

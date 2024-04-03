---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:38.535051-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, b\u1EA1n c\xF3 th\u1EC3\
  \ t\u1EA1o ra s\u1ED1 ng\u1EABu nhi\xEAn s\u1EED d\u1EE5ng h\xE0m `Math.random()`,\
  \ t\u01B0\u01A1ng t\u1EF1 nh\u01B0 trong JavaScript. H\xE0m n\xE0y tr\u1EA3 v\u1EC1\
  \ m\u1ED9t s\u1ED1\u2026"
lastmod: '2024-03-13T22:44:36.034407-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, b\u1EA1n c\xF3 th\u1EC3 t\u1EA1o ra s\u1ED1 ng\u1EAB\
  u nhi\xEAn s\u1EED d\u1EE5ng h\xE0m `Math.random()`, t\u01B0\u01A1ng t\u1EF1 nh\u01B0\
  \ trong JavaScript."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm thế nào:
Trong Google Apps Script, bạn có thể tạo ra số ngẫu nhiên sử dụng hàm `Math.random()`, tương tự như trong JavaScript. Hàm này trả về một số nổi, giả ngẫu nhiên nằm trong phạm vi từ 0 (bao gồm) đến 1 (không bao gồm). Để điều chỉnh các số này cho các trường hợp sử dụng khác nhau, như tạo ra các số nguyên trong phạm vi cụ thể, bạn có thể cần thực hiện thêm các phép tính.

### Tạo ra Một Số Ngẫu Nhiên Cơ Bản
Để tạo ra một số ngẫu nhiên đơn giản và ghi nó vào bảng điều khiển:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Kết quả mẫu:* `0.1234567890123456`

### Tạo ra Một Số Nguyên trong Phạm Vi Cụ Thể
Để tạo ra một số nguyên ngẫu nhiên giữa hai giá trị (`min` và `max`), bao gồm cả hai:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Ví dụ:
getRandomInt(1, 10);
```
*Kết quả mẫu*: `7`

Nhớ rằng, hàm `Math.ceil()` được sử dụng để làm tròn giá trị nhỏ nhất lên, và `Math.floor()` được sử dụng để làm tròn giá trị lớn nhất xuống, bảo đảm rằng số ngẫu nhiên nằm trong phạm vi đã chỉ định.

## Sâu hơn
Cơ chế tạo ra số ngẫu nhiên trong Google Apps Script, và thực sự trong hầu hết các ngôn ngữ lập trình, sử dụng bộ tạo số ngẫu nhiên giả (PRNG). Kỹ thuật này là có tính quyết định và phụ thuộc vào một giá trị ban đầu, được biết đến như giống, để tạo ra một chuỗi số trông có vẻ ngẫu nhiên. Mặc dù đủ cho nhiều ứng dụng, điều quan trọng cần biết là số ngẫu nhiên giả có thể không phù hợp ở nơi mà an ninh cao hoặc ngẫu nhiên thực sự được yêu cầu, như trong các ứng dụng mật mã.

Ngẫu nhiên thực sự có thể đạt được qua bộ tạo số ngẫu nhiên phần cứng hoặc các dịch vụ tạo ra ngẫu nhiên từ các hiện tượng tự nhiên. Tuy nhiên, cho hầu hết các nhu cầu lập trình hàng ngày trong Google Apps Script, `Math.random()` là đủ.

Trong lịch sử, việc tìm kiếm các kỹ thuật tạo số ngẫu nhiên hiệu quả hơn đã dẫn đến sự phát triển của nhiều thuật toán, với các ví dụ đ notable áng chú ý là Mersenne Twister và Linear Congruential Generator (LCG). Tuy nhiên, do mức độ trừu tượng cao trong Google Apps Script, hầu hết người dùng không cần phải triển khai trực tiếp các thuật toán này nhưng hiểu về các nguyên tắc cơ bản có thể giúp đánh giá cao tầm quan trọng và giới hạn của việc tạo số ngẫu nhiên trong các kịch bản của bạn.

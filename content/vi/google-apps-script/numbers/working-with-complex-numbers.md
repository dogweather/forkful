---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:41.835603-07:00
description: "L\xE0m th\u1EBF n\xE0o: Google Apps Script kh\xF4ng h\u1ED7 tr\u1EE3\
  \ s\u1EB5n cho s\u1ED1 ph\u1EE9c, bu\u1ED9c ph\u1EA3i tri\u1EC3n khai ch\u1EE9c\
  \ n\u0103ng t\xF9y ch\u1EC9nh. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\u1EA5u tr\xFA\
  c c\u01A1 b\u1EA3n \u0111\u1EC3 x\u1EED l\xFD s\u1ED1 ph\u1EE9c, bao\u2026"
lastmod: '2024-03-13T22:44:36.031706-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script kh\xF4ng h\u1ED7 tr\u1EE3 s\u1EB5n cho s\u1ED1 ph\u1EE9\
  c, bu\u1ED9c ph\u1EA3i tri\u1EC3n khai ch\u1EE9c n\u0103ng t\xF9y ch\u1EC9nh."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Google Apps Script không hỗ trợ sẵn cho số phức, buộc phải triển khai chức năng tùy chỉnh. Dưới đây là cấu trúc cơ bản để xử lý số phức, bao gồm cộng, trừ và nhân.

```javascript
// Định nghĩa một constructor cho số phức
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Phương thức để cộng hai số phức
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Phương thức để trừ hai số phức
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Phương thức để nhân hai số phức
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Ví dụ sử dụng
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Cộng hai số phức
var sum = num1.add(num2);
console.log(`Tổng: ${sum.real} + ${sum.imag}i`); // Tổng: 4 + 6i

// Trừ hai số phức
var difference = num1.subtract(num2);
console.log(`Hiệu: ${difference.real} + ${difference.imag}i`); // Hiệu: 2 + 2i

// Nhân hai số phức
var product = num1.multiply(num2);
console.log(`Tích: ${product.real} + ${product.imag}i`); // Tích: -5 + 10i
```

## Sâu rộng hơn:
Khái niệm về số phức có từ thế kỷ 16, nhưng công việc của các nhà toán học như Euler và Gauss đã củng cố vị thế của chúng trong toán học. Mặc dù chúng rất hữu ích, số phức không được hỗ trợ trực tiếp trong JavaScript hoặc, do đó, Google Apps Script. Sự thiếu hỗ trợ này có nghĩa là các thao tác trên số phức phải được lập trình thủ công, như đã trình bày. Mặc dù điều này cung cấp một cơ hội học tốt và tính năng đủ cho nhu cầu cơ bản, cho công việc tính toán nặng nề đòi hỏi sử dụng số phức, người ta có thể xem xét sử dụng các môi trường lập trình khác phù hợp hơn với tính toán toán học, như Python kèm theo NumPy, những cái cung cấp thao tác tối ưu, sẵn có và cao cấp để xử lý số phức. Tuy nhiên, hiểu và triển khai các thao tác cơ bản trong Google Apps Script là một bài tập hữu ích cho những người muốn mở rộng kỹ năng lập trình của mình và áp dụng chúng trong nhiều ngữ cảnh khác nhau.

---
title:                "Làm việc với số phức"
aliases: - /vi/google-apps-script/working-with-complex-numbers.md
date:                  2024-02-01T22:07:41.835603-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Số phức, được biểu diễn như là sự kết hợp của phần thực và phần ảo (ví dụ, 3 + 4i), là cơ bản trong nhiều vấn đề tính toán, đặc biệt trong kỹ thuật, vật lý và toán ứng dụng. Học cách thao tác với những số này trong Google Apps Script cho phép lập trình viên mở rộng khả năng của họ vào tính toán khoa học, xử lý tín hiệu, và hơn thế nữa.

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

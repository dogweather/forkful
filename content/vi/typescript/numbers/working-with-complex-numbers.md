---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:51.456967-07:00
description: "S\u1ED1 ph\u1EE9c, bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o (th\u01B0\u1EDDng \u0111\u01B0\u1EE3c vi\u1EBFt d\u01B0\u1EDB\
  i d\u1EA1ng a + bi), m\u1EDF ra kh\u1EA3 n\u0103ng t\xEDnh to\xE1n kh\xF4ng th\u1EF1\
  c t\u1EBF ho\u1EB7c kh\xF4ng th\u1EC3 th\u1EF1c hi\u1EC7n ch\u1EC9 v\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.310744-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c, bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o (th\u01B0\u1EDDng \u0111\u01B0\u1EE3c vi\u1EBFt d\u01B0\u1EDB\
  i d\u1EA1ng a + bi), m\u1EDF ra kh\u1EA3 n\u0103ng t\xEDnh to\xE1n kh\xF4ng th\u1EF1\
  c t\u1EBF ho\u1EB7c kh\xF4ng th\u1EC3 th\u1EF1c hi\u1EC7n ch\u1EC9 v\u1EDBi\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào & Tại sao?
Số phức, bao gồm một phần thực và một phần ảo (thường được viết dưới dạng a + bi), mở ra khả năng tính toán không thực tế hoặc không thể thực hiện chỉ với số thực. Các lập trình viên sử dụng chúng trong các lĩnh vực như xử lý tín hiệu, tính toán lượng tử và toán ứng dụng, nơi mà biểu diễn số hai chiều là cần thiết.

## Cách thực hiện:
Xử lý số phức trong TypeScript đòi hỏi một lớp dédicac. Hãy tạo một lớp và làm việc qua phép cộng và nhân.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Tổng: ${sum.toString()}`); // Kết quả: Tổng: 4 + 6i
console.log(`Tích: ${product.toString()}`); // Kết quả: Tích: -5 + 10i
```

## Tìm hiểu sâu
Trong lịch sử, số phức đã từng là chủ đề gây tranh cãi - thậm chí được gọi là 'ảo' để thể hiện sự hoài nghi ban đầu. Bây giờ, chúng là nền tảng trong toán học và khoa học hiện đại.

Các lựa chọn khác cho lớp đơn giản của chúng tôi có thể bao gồm sử dụng các thư viện hiện có như `math.js` hoặc `complex.js`, được chi tiết hóa với các tính năng bổ sung như các hàm lượng giác, lũy thừa, và liên hợp phức.

Chi tiết thực hiện trong TypeScript của chúng tôi tóm tắt là định nghĩa các phép toán số học. Phương pháp `add` đơn giản cộng các phần tương ứng. `multiply` áp dụng phương pháp FOIL được sử dụng trong đại số, nhớ rằng `i^2 = -1`.

## Xem thêm
Để tìm hiểu thêm và nguồn tài nguyên về số phức và việc sử dụng chúng trong lập trình, hãy xem:

- Đại số Số phức MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Thư viện `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- Thư viện `complex.js`: https://complex-js.github.io/complex.js/

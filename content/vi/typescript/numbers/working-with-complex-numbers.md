---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:51.456967-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: X\u1EED l\xFD s\u1ED1 ph\u1EE9c trong TypeScript\
  \ \u0111\xF2i h\u1ECFi m\u1ED9t l\u1EDBp d\xE9dicac. H\xE3y t\u1EA1o m\u1ED9t l\u1EDB\
  p v\xE0 l\xE0m vi\u1EC7c qua ph\xE9p c\u1ED9ng v\xE0 nh\xE2n."
lastmod: '2024-03-13T22:44:36.310744-06:00'
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD s\u1ED1 ph\u1EE9c trong TypeScript \u0111\xF2i h\u1ECFi m\u1ED9\
  t l\u1EDBp d\xE9dicac."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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

---
title:                "Làm việc với số phức"
aliases:
- vi/typescript/working-with-complex-numbers.md
date:                  2024-01-28T22:12:51.456967-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

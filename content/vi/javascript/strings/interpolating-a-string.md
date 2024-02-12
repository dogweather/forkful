---
title:                "Nội suy chuỗi ký tự"
aliases:
- /vi/javascript/interpolating-a-string.md
date:                  2024-01-28T22:02:11.593838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?

Nội suy chuỗi là cách để nhúng trực tiếp các biến vào bên trong một chuỗi. Lập trình viên sử dụng nó để nối các biến và chuỗi một cách hiệu quả, làm cho code dễ đọc và dễ bảo trì hơn.

## Làm thế nào:

Trong JavaScript, nội suy chuỗi thường được thực hiện bằng cách sử dụng các literal mẫu. Dưới đây là cách bạn có thể làm điều đó:

```javascript
const name = 'Alice';
const message = `Xin chào, ${name}! Hôm nay bạn thế nào?`;
console.log(message); // Đầu ra: Xin chào, Alice! Hôm nay bạn thế nào?
```

Bạn cũng có thể thực hiện các phép toán bên trong các vị trí giữ chỗ:

```javascript
const a = 10;
const b = 5;
console.log(`Mười nhân năm là ${a * b}.`); // Đầu ra: Mười nhân năm là 50.
```

## Sâu hơn nữa

Về mặt lịch sử, nội suy chuỗi không hẳn là điều dễ dàng trong JavaScript. Trước ES6 (ECMAScript 2015), việc nối chuỗi thường được thực hiện bằng cách sử dụng toán tử `+`:

```javascript
var name = 'Bob';
var message = 'Xin chào, ' + name + '! Hôm nay bạn thế nào?';
```

Với sự ra đời của ES6, các literal mẫu (nằm giữa dấu ngoặc ngược \` \`) xuất hiện, mang theo cú pháp dễ dàng hơn với các vị trí giữ chỗ `${}`.

Các phương pháp thay thế cho nội suy chuỗi bao gồm nối chuỗi bằng toán tử `+` và phương thức `concat()`, hoặc sử dụng các hàm giống như `sprintf` từ thư viện của bên thứ ba.

Hiệu suất của các literal mẫu nói chung tương đương với những phương pháp cũ hơn. Tuy nhiên, khả năng đọc và khả năng bao gồm biểu thức (như `${a * b}`) trong chuỗi làm cho các literal mẫu trở thành lựa chọn mạnh mẽ cho các nhà phát triển.

## Xem thêm

- MDN về Literal Mẫu: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Nối chuỗi trong JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators
- Lịch sử mô-đun JavaScript "ECMAScript": https://www.ecma-international.org/publications-and-standards/standards/ecma-262/

---
title:                "Nối chuỗi ký tự"
aliases:
- /vi/javascript/concatenating-strings/
date:                  2024-01-28T21:57:37.374698-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối. Chúng ta làm vậy để tạo ra các thông điệp, URL, hoặc bất kỳ văn bản nào với các phần từ nguồn khác nhau.

## Làm thế nào:
Trong JavaScript, bạn có một vài cách để nối chuỗi. Kiểu cổ điển: `+`. Kiểu hiện đại: template literals. Dưới đây là cách chúng trông như thế nào.

**Sử dụng toán tử +:**
```javascript
let hello = "Hello, ";
let world = "world!";
let greeting = hello + world; 
console.log(greeting); // "Hello, world!"
```

**Sử dụng template literals:**
```javascript
let user = "Jane";
let welcomeMessage = `Hi, ${user}! Welcome back.`;
console.log(welcomeMessage); // "Hi, Jane! Welcome back."
```

## Đào Sâu
Ngày xưa, `+` là cách để thực hiện, nhưng nó trở nên rối rắm với nhiều biến. Vào năm 2015, ES6 được giới thiệu, bao gồm template literals (những dấu backtick `\``). Điều này có nghĩa là tạo ra những chuỗi có vẻ ngoài sạch sẽ hơn và khả năng thả biến và biểu thức ngay vào trong chuỗi của bạn mà không mất công sức.

**Tại sao `+` có thể gây đau đầu:**
- Khó đọc với nhiều biến.
- Dễ bỏ qua khoảng trắng, dẫn đến từ bị dính vào nhau.
- Hơn nữa, ai cần tất cả những dấu cộng đó?

**Tại sao template literals tuyệt vời:**
- Đọc dễ dàng: Như một câu tiếng Anh với những chỗ trống được điền vào.
- Hỗ trợ đa dòng: Bạn có thể tạo chuỗi trải qua nhiều dòng mà không cần `+` hoặc `\n`.
- Chèn biểu thức: Thêm biến, tính toán, tất cả trong một lần.

**Đây là ví dụ đa dòng và biểu thức trong hành động:**
```javascript
let apples = 3;
let oranges = 5;
let fruitSummary = `Bạn có ${apples + oranges} miếng trái cây:
${apples} táo và
${oranges} cam.`;
console.log(fruitSummary);
```
Xuất ra một bản tóm tắt được định dạng gọn gàng mà không cần bất kỳ kỹ thuật `+` nào.

Về mặt kỹ thuật, việc nối chuỗi tạo ra một chuỗi mới mỗi khi bạn sử dụng `+`. Đối với máy tính, đó giống như tạo ra một thanh kẹo mới mỗi khi bạn chỉ muốn thêm một hạt lạc. Không hiệu quả lắm. Template literals giống như việc có một khuôn mẫu nơi bạn có thể bỏ tất cả các nguyên liệu vào cùng một lúc - hiệu suất tốt hơn, đặc biệt với các chuỗi lớn hoặc trong vòng lặp.

## Xem thêm
- MDN Web Docs về template literals (để đọc thêm): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Phương thức và thuộc tính chuỗi (hữu ích khi xử lý chuỗi): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String

---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases: - /vi/javascript/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:14.714545-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Chuyển đổi một chuỗi thành chữ thường nghĩa là biến đổi tất cả các ký tự trong đó thành dạng chữ thường của chúng. Lập trình viên thực hiện việc này để đảm bảo tính nhất quán, đặc biệt là cho các so sánh không phân biệt chữ hoa chữ thường, như khi chuẩn hóa đầu vào của người dùng hoặc tìm kiếm trong nội dung văn bản.

## Cách thực hiện:
Trong JavaScript, chúng ta chuyển đổi một chuỗi sang chữ thường bằng phương thức `.toLowerCase()`. Nó đơn giản như sau:

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hello, world!"
```

Khi sử dụng, mỗi ký tự trong chuỗi gốc (nếu có thể) sẽ được chuyển đổi thành chữ thường:

```javascript
let mixedCase = "jAvAScript ROCKs!";
let lowerCased = mixedCase.toLowerCase();
console.log(lowerCased); // "javascript rocks!"
```

Lưu ý rằng các ký tự không có dạng chữ thường tương đương sẽ giữ nguyên không đổi.

## Đi sâu hơn
Ngày xưa, việc xử lý văn bản cần phải cẩn thận với mã hóa ký tự và chuyển đổi thủ công. Nhưng trong JavaScript hiện đại, `.toLowerCase()` giúp ẩn đi những phức tạp đó. Phía dưới, nó sử dụng bảng ánh xạ Unicode để chuyển đổi ký tự, vì vậy nó hoạt động không chỉ với A-Z.

Có những phương thức khác tồn tại, như:

- `toLocaleLowerCase()`: Điều này tôn trọng ngữ cảnh ngôn ngữ của người dùng, làm cho nó trở nên thiết yếu cho một số ngôn ngữ nơi quy tắc viết thường phụ thuộc vào ngữ cảnh.

- Biểu thức chính quy (Regular expressions): Trước khi có `toLowerCase()`, các nhà phát triển có thể đã sử dụng regex để thủ công thay thế các ký tự chữ hoa.

Về chi tiết, nhớ rằng `.toLowerCase()` không thay đổi chuỗi gốc (chuỗi trong JavaScript là bất biến). Bạn luôn nhận được một chuỗi mới. Nó cũng xử lý tất cả các ký tự được Unicode tiêu chuẩn nhận diện là chữ hoa, điều này nghĩa là bạn được bảo vệ trên các ngôn ngữ và kịch bản khác nhau.

## Xem thêm
- [Tài liệu MDN web về toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Tiêu chuẩn Unicode cho chữ hoa và chữ thường](https://unicode.org/reports/tr21/tr21-5.html)
- [Chữ hoa và chữ thường với Ngữ cảnh: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)

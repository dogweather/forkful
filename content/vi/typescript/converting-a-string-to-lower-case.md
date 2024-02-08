---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- vi/typescript/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:14.136575-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm thế nào & Tại sao?
Chuyển đổi một chuỗi sang chữ thường có nghĩa là làm cho mỗi ký tự trong chuỗi trở thành chữ cái nhỏ. Các lập trình viên thực hiện điều này để đảm bảo tính nhất quán, đặc biệt là đối với các so sánh không phân biệt chữ hoa chữ thường, chẳng hạn như khi kiểm tra đầu vào của người dùng so với một danh sách các lệnh hoặc dữ liệu đã lưu trữ.

## Cách thực hiện:
Trong TypeScript, việc chuyển đổi một chuỗi sang chữ thường là việc rất đơn giản. Chỉ cần gọi `.toLowerCase()` cho chuỗi của bạn. Dưới đây là cách thực hiện:

```typescript
let myString: string = "HeLLo, WorLD!";
let lowerCaseString: string = myString.toLowerCase();
console.log(lowerCaseString); // Kết quả: "hello, world!"
```

Dễ thôi, phải không?

## Sâu hơn nữa
Ngày xưa, việc xử lý văn bản không luôn luôn nhất quán, và mã hóa ký tự có thể là một miền không lệ. Bây giờ, với Unicode và các phương pháp chuẩn hóa, các trường hợp chữ viết được thống nhất qua các ngôn ngữ. So với `.toLowerCase()`, một phương pháp cổ điển (như thao tác ASCII) giống như thời kỳ đồ đá. Các phương pháp thay thế (như `.toLocaleLowerCase()`) xem xét các quy tắc cụ thể của địa phương cho việc viết hoa chính xác, có thể rất tiện lợi. Bên dưới lớp vỏ, `.toLowerCase()` trong JavaScript (và mở rộng trong TypeScript) đi qua mỗi ký tự và, nếu đó là một chữ cái in hoa, chuyển đổi nó thành tương đương chữ thường dựa trên bản đồ Unicode.

## Xem thêm
Để biết thêm về các kỹ thuật xử lý chuỗi và để làm phong phú thêm trò chơi xử lý văn bản của bạn, hãy xem những tài liệu sau:

- Tài liệu MDN về `.toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Tài liệu chính thức của TypeScript: [TypeScriptlang.org](https://www.typescriptlang.org/docs/)
- Để hiểu rõ hơn về chuyển đổi cụ thể theo địa phương: [MDN toLocaleLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Để tìm hiểu sâu về tiêu chuẩn Unicode: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)

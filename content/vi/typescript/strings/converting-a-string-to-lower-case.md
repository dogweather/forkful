---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:14.136575-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\
  \u1EDDng c\xF3 ngh\u0129a l\xE0 l\xE0m cho m\u1ED7i k\xFD t\u1EF1 trong chu\u1ED7\
  i tr\u1EDF th\xE0nh ch\u1EEF c\xE1i nh\u1ECF. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o\u2026"
lastmod: '2024-03-13T22:44:36.301564-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDD\
  ng c\xF3 ngh\u0129a l\xE0 l\xE0m cho m\u1ED7i k\xFD t\u1EF1 trong chu\u1ED7i tr\u1EDF\
  \ th\xE0nh ch\u1EEF c\xE1i nh\u1ECF. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\
  \ hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
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

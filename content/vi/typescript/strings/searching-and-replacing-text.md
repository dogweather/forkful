---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:52.787732-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: TypeScript, x\xE2y d\u1EF1ng d\u1EF1a tr\xEA\
  n JavaScript, \u0111i k\xE8m v\u1EDBi c\xE1c ph\u01B0\u01A1ng ph\xE1p ti\u1EC7n\
  \ l\u1EE3i cho vi\u1EC7c manipulasi chu\u1ED7i. Ch\xFAng ta c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.299088-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, x\xE2y d\u1EF1ng d\u1EF1a tr\xEAn JavaScript, \u0111i k\xE8\
  m v\u1EDBi c\xE1c ph\u01B0\u01A1ng ph\xE1p ti\u1EC7n l\u1EE3i cho vi\u1EC7c manipulasi\
  \ chu\u1ED7i."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Cách thực hiện:
TypeScript, xây dựng dựa trên JavaScript, đi kèm với các phương pháp tiện lợi cho việc manipulasi chuỗi. Chúng ta có thể sử dụng `String.prototype.replace()` cho các hoạt động tìm kiếm và thay thế cơ bản. Hãy xem các đoạn mã sau:

```typescript
// Thay thế chuỗi đơn giản
let text: string = "Hello, World!";
let newText: string = text.replace("World", "TypeScript");
console.log(newText);  // Kết quả: Hello, TypeScript!

// Thay thế toàn cầu với regex
let regexText: string = "foo bar foo bar";
let globalRegex: RegExp = /foo/g;
let newRegexText: string = regexText.replace(globalRegex, "baz");
console.log(newRegexText);  // Kết quả: baz bar baz bar

// Thay thế với một hàm
let dynamicText: string = "I have 2 apples and 5 oranges.";
let fruitCounter: string = dynamicText.replace(/\d+/g, (match) => {
    return (+match * 2).toString();
});
console.log(fruitCounter);  // Kết quả: Tôi có 4 quả táo và 10 quả cam.
```

## Sâu hơn
Lịch sử, việc thay thế văn bản đã là một tính năng trong các công cụ xử lý văn bản ngay từ đầu, với các công cụ Unix như `sed` là ví dụ điển hình. Trong lập trình hiện đại hơn, các hoạt động thay thế thường mạnh mẽ hơn khi kết hợp với biểu thức chính quy (regex) để khớp mẫu.

Các lựa chọn thay thế cho `String.prototype.replace()` trong TypeScript là nhiều. Các thư viện như Lodash cung cấp `_.replace()` với cú pháp tương tự. Đối với các kịch bản tiên tiến hơn, bạn có thể xem xét xây dựng bộ phân tích cú pháp của riêng mình hoặc sử dụng các thư viện phân tích cú pháp cho các nhiệm vụ biến đổi vượt ra ngoài việc thay thế chuỗi đơn giản.

Khi nói đến việc thực hiện, nhớ rằng `.replace()` sẽ không thay đổi chuỗi gốc. Chuỗi trong JavaScript và TypeScript là bất biến. Phương pháp trả về một chuỗi mới, vì vậy nếu bạn cần văn bản đã chỉnh sửa, bạn sẽ phải lưu trữ nó, như trong các ví dụ ở trên.

## Xem thêm
- Tài liệu MDN Web về `replace()`: [MDN String replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Công cụ kiểm tra Regex để nâng cao kỹ năng khớp mẫu của bạn: [Regex101](https://regex101.com/)
- Thay thế chuỗi của Lodash cho một cách tiếp cận khác: [Lodash _.replace](https://lodash.com/docs/4.17.15#replace)

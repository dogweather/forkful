---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:52.787732-07:00
description: "Vi\u1EC7c t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong\
  \ chu\u1ED7i l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBFn trong l\u1EAD\
  p tr\xECnh, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3\
  \ x\u1EED l\xFD v\xE0 thao t\xE1c d\u1EEF li\u1EC7u. \u0110i\u1EC1u n\xE0y r\u1EA5\
  t\u2026"
lastmod: '2024-03-13T22:44:36.299088-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong chu\u1ED7\
  i l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBFn trong l\u1EADp tr\xECnh,\
  \ th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 x\u1EED l\xFD\
  \ v\xE0 thao t\xE1c d\u1EEF li\u1EC7u."
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

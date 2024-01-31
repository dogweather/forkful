---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:52.787732-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm kiếm và thay thế văn bản trong chuỗi là một nhiệm vụ phổ biến trong lập trình, thường được sử dụng để xử lý và thao tác dữ liệu. Điều này rất quan trọng để tinh chỉnh nội dung, sửa lỗi, và tự động hóa chỉnh sửa trên các cơ sở mã lớn hoặc bộ dữ liệu.

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

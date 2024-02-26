---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:19.939418-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu bao\
  \ g\u1ED3m vi\u1EC7c t\xECm ki\u1EBFm trong m\u1ED9t chu\u1ED7i cho m\u1ED9t d\xE3\
  y k\xFD t\u1EF1 c\u1EE5 th\u1EC3 (m\u1EABu) v\xE0 lo\u1EA1i b\u1ECF ch\xFAng. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
lastmod: '2024-02-25T18:49:34.622730-07:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu bao g\u1ED3\
  m vi\u1EC7c t\xECm ki\u1EBFm trong m\u1ED9t chu\u1ED7i cho m\u1ED9t d\xE3y k\xFD\
  \ t\u1EF1 c\u1EE5 th\u1EC3 (m\u1EABu) v\xE0 lo\u1EA1i b\u1ECF ch\xFAng. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xóa các ký tự khớp với một mẫu bao gồm việc tìm kiếm trong một chuỗi cho một dãy ký tự cụ thể (mẫu) và loại bỏ chúng. Các lập trình viên thực hiện điều này để làm sạch hoặc biến đổi dữ liệu văn bản – hãy nghĩ đến việc loại bỏ các thẻ HTML khỏi chuỗi, hoặc loại bỏ dấu câu không mong muốn.

## Làm thế nào:

```TypeScript
function deletePattern(text: string, pattern: string): string {
  // Tạo một RegExp từ chuỗi mẫu
  const regex = new RegExp(pattern, 'g');
  // Thay thế các lần xuất hiện của mẫu bằng chuỗi trống
  return text.replace(regex, '');
}

// Ví dụ sử dụng
const originalText = "Hello, World! This -- is a test.";
const newText = deletePattern(originalText, "[,\\-!]");
console.log(newText);  // Đầu ra: "Hello World This  is a test"
```

## Đi sâu vào vấn đề

Xử lý chuỗi trong lập trình có thể truy nguyên về thời kỳ bình minh của máy tính. Trong TypeScript, dựa trên JavaScript, việc thao tác với chuỗi là một công việc hàng ngày. Hàm `replace()` mà chúng ta đã sử dụng được kế thừa từ kho vũ khí thao tác chuỗi mạnh mẽ của JavaScript.

Có những phương pháp thay thế cho RegExp trong việc khớp mẫu – đôi khi bạn có thể muốn lặp qua từng ký tự và đưa ra quyết định với một câu lệnh switch hoặc một loạt câu lệnh if. Nhưng biểu thức chính quy cung cấp một cách mô tả mẫu phức tạp một cách ngắn gọn và mạnh mẽ.

Chi tiết thực hiện trở nên thú vị khi bạn khám phá cách các mẫu RegExp được diễn giải tại thời điểm chạy. Cờ 'g' trong bộ xây dựng RegExp bảo cho công cụ tìm kiếm một cách toàn cầu qua chuỗi. Nếu không có nó, chỉ lần xuất hiện đầu tiên sẽ được thay thế. Các biểu thức chính quy có thể đơn giản hoặc phức tạp đến không ngờ, tùy thuộc vào nhu cầu của bạn.

## Xem thêm

- MDN Web Docs về RegExp: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- Sổ tay TypeScript về việc thao tác chuỗi: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Kiểm tra biểu thức chính quy để giúp tạo mẫu: [https://regexr.com/](https://regexr.com/)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:34.350628-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t s\u1ED1\
  \ TypeScript nhanh ch\xF3ng \u0111\u1EC3 b\u1EA1n b\u1EAFt \u0111\u1EA7u vi\u1EBF\
  t hoa c\xE1c chu\u1ED7i."
lastmod: '2024-03-13T22:44:36.296446-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t s\u1ED1 TypeScript nhanh ch\xF3\
  ng \u0111\u1EC3 b\u1EA1n b\u1EAFt \u0111\u1EA7u vi\u1EBFt hoa c\xE1c chu\u1ED7i."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Dưới đây là một số TypeScript nhanh chóng để bạn bắt đầu viết hoa các chuỗi:

```typescript
function capitalizeString(input: string): string {
  return input.replace(/\w\S*/g, (word) => {
    return word.charAt(0).toUpperCase() + word.substr(1).toLowerCase();
  });
}

// Ví dụ sử dụng:
const title = "hello world from TypeScript";
const capitalizedTitle = capitalizeString(title);
console.log(capitalizedTitle); // Kết quả: "Hello World From Typescript"
```

Dễ dàng, phải không? Bây giờ hãy biến những chuỗi chữ thường thành cái gì đó đẹp đẽ!

## Đào Sâu
Việc viết hoa đã tồn tại từ thời của các kịch bản cổ đại, giúp tăng tính dễ đọc. Trong lập trình, ngoài vẻ đẹp thẩm mỹ và sự chính xác về mặt ngữ pháp, việc viết hoa chuỗi có thể rất quan trọng đối với các thao tác so sánh khi "Apple" và "apple" có thể được xử lý khác nhau.

Các phương pháp thay thế cho hàm `capitalizeString` có thể bao gồm việc sử dụng các thư viện như Lodash, cung cấp phương thức `_.startCase`, hoặc dựa vào CSS cho việc viết hoa trực quan (`text-transform: capitalize;`). Tuy nhiên, CSS không thay đổi giá trị thực sự của chuỗi, chỉ là cách hiển thị.

JavaScript ban đầu không bao gồm một phương pháp tích hợp sẵn để viết hoa chuỗi, để lại điều này cho sự sáng tạo của các lập trình viên. Hàm ở trên sử dụng một biểu thức chính quy để nhận biết ranh giới từ `\w\S*`, viết hoa chữ cái đầu tiên với `toUpperCase()`, và phần còn lại với `toLowerCase()`.

## Xem Thêm
- Tài liệu về Chuỗi MDN: [https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String)
- Hàm `_.startCase` của Lodash: [https://lodash.com/docs/#startCase](https://lodash.com/docs/#startCase)
- String.prototype.toLocaleUpperCase (cho các biến đổi nhạy cảm với ngữ cảnh): [https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase](https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)

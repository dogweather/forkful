---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:34.350628-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 bi\u1EBF\
  n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED7i t\u1EEB th\xE0nh ch\u1EEF\
  \ hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDDng. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3\
  o\u2026"
lastmod: '2024-02-25T18:49:34.621079-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 bi\u1EBFn ch\u1EEF\
  \ c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED7i t\u1EEB th\xE0nh ch\u1EEF hoa v\xE0\
  \ ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDDng. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết hoa một chuỗi nghĩa là biến chữ cái đầu tiên của mỗi từ thành chữ hoa và phần còn lại thành chữ thường. Lập trình viên làm điều này để đảm bảo tính nhất quán trong định dạng trên các giao diện người dùng và để chắc chắn rằng tên riêng và tiêu đề được hiển thị một cách chính xác.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:41.870641-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ph\xE2n t\xEDch c\xFA ph\xE1p JSON trong JavaScript."
lastmod: '2024-03-13T22:44:37.180975-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p JSON trong JavaScript."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Phân tích cú pháp JSON trong JavaScript:
```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const user = JSON.parse(jsonString);
console.log(user.name); // Đầu ra: John
```

Biến một đối tượng JavaScript thành JSON:
```javascript
const user = { name: 'John', age: 30, city: 'New York' };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Đầu ra: '{"name":"John","age":30,"city":"New York"}'
```

## Sâu hơn
JSON được phái sinh từ JavaScript nhưng hiện nay đã trở thành một định dạng độc lập với ngôn ngữ. Nhiều phương thức thay thế như XML tồn tại, nhưng cú pháp tối giản của JSON đã giành được sự phổ biến cho các payload API. Về mặt kỹ thuật, JSON là một tập con của ký hiệu đối tượng literal trong JavaScript với một số điểm khác biệt, chẳng hạn như yêu cầu khóa phải được đặt trong dấu ngoặc kép.

## Xem thêm
- MDN JSON: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
- Định dạng & Kiểm tra JSON: https://jsonlint.com/
- JSON so với XML: https://www.w3schools.com/js/js_json_xml.asp

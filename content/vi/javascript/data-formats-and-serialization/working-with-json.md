---
aliases:
- /vi/javascript/working-with-json/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:41.870641-07:00
description: "JSON, hay JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECBnh\
  \ d\u1EA1ng d\u1EEF li\u1EC7u nh\u1EB9 \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 truy\u1EC1\
  n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3\
  \ v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc/vi\u1EBFt cho con\u2026"
lastmod: 2024-02-18 23:08:51.159765
model: gpt-4-0125-preview
summary: "JSON, hay JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u nh\u1EB9 \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 truy\u1EC1n t\u1EA3\
  i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC n\xF3\
  \ d\u1EC5 \u0111\u1ECDc/vi\u1EBFt cho con\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
JSON, hay JavaScript Object Notation, là một định dạng dữ liệu nhẹ để lưu trữ và truyền tải dữ liệu. Lập trình viên sử dụng nó vì nó dễ đọc/viết cho con người và máy móc có thể phân tích và tạo ra một cách nhanh chóng.

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

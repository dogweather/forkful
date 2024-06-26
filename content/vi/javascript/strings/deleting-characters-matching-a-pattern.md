---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:01.005481-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: S\u1EED d\u1EE5ng `replace()` v\u1EDBi\
  \ bi\u1EC3u th\u1EE9c ch\xEDnh quy. C\u1EDD `g` thay th\u1EBF t\u1EA5t c\u1EA3 c\xE1\
  c ph\u1EA7n tr\xF9ng kh\u1EDBp, kh\xF4ng ch\u1EC9 ph\u1EA7n \u0111\u1EA7u ti\xEA\
  n."
lastmod: '2024-03-13T22:44:37.132240-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng `replace()` v\u1EDBi bi\u1EC3u th\u1EE9c ch\xEDnh quy."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Cách thực hiện:
Sử dụng `replace()` với biểu thức chính quy. Cờ `g` thay thế tất cả các phần trùng khớp, không chỉ phần đầu tiên.

```javascript
let message = "S0m3 messy-string_with! unwanted characters.";
let cleanMessage = message.replace(/[0-9_!-]/g, '');
console.log(cleanMessage); // Kết quả: "Sm messystringwith unwanted characters."
```

## Sâu hơn
JavaScript đã lâu dùng biểu thức chính quy (`RegExp`) cho việc khớp mẫu. Hàm `replace()` là công cụ của bạn để chỉnh sửa chuỗi kể từ khi ngôn ngữ này ra đời. Có các phương thức khác như `split()` và `join()` hoặc sử dụng vòng lặp để tái cấu trúc chuỗi, nhưng không mạch lạc như vậy.

Dưới đây là một số điểm chú ý:
- Sử dụng `replace()` cho các giải pháp một dòng đơn giản.
- Biểu thức chính quy cung cấp khả năng khớp mẫu mạnh mẽ.
- Lưu ý về hiệu suất của `RegExp` trong các vòng lặp chặt chẽ hoặc chuỗi cực lớn.

Nói về phương pháp hiện đại: mẫu như `/[^a-z]/gi` loại bỏ bất cứ thứ gì không phải là chữ cái, tôn trọng sự không phân biệt chữ hoa chữ thường với cờ `i`. Sự ra đời của các ký tự mẫu trong ECMAScript 2015 đã làm cho việc thay thế phức tạp trở nên dễ dàng hơn, nâng cao khả năng đọc.

Biểu thức chính quy vẫn làm cho một số lập trình viên cảm thấy e ngại do độ phức tạp của cú pháp. Tuy nhiên, với sự phát triển của JavaScript hiện đại, các công cụ và phương pháp như các hàm thao tác chuỗi (`trim()`, `padStart()`, `padEnd()`, v.v.) đều sẵn có để làm đơn giản các công việc thông thường, có thể không cần đến regex.

## Xem thêm
- [Tài liệu MDN về replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExr: Học, xây dựng, & kiểm tra RegEx](https://regexr.com/)
- [Tài liệu Tham khảo RegExp JavaScript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)

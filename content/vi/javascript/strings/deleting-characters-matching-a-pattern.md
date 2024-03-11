---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:01.005481-07:00
description: "X\xF3a b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EF1a tr\xEAn m\u1ED9t m\u1EAB\
  u l\xE0m cho chu\u1ED7i tr\u1EDF n\xEAn s\u1EA1ch s\u1EBD v\xE0 \u0111\u1ED3ng nh\u1EA5\
  t. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 \u0111\u1ECBnh d\u1EA1ng, lo\u1EA1i b\u1ECF c\xE1c k\xFD t\u1EF1 kh\xF4ng\u2026"
lastmod: '2024-03-11T00:14:10.438117-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EF1a tr\xEAn m\u1ED9t m\u1EABu l\xE0\
  m cho chu\u1ED7i tr\u1EDF n\xEAn s\u1EA1ch s\u1EBD v\xE0 \u0111\u1ED3ng nh\u1EA5\
  t. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 \u0111\u1ECBnh d\u1EA1ng, lo\u1EA1i b\u1ECF c\xE1c k\xFD t\u1EF1 kh\xF4ng\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Xóa bỏ các ký tự dựa trên một mẫu làm cho chuỗi trở nên sạch sẽ và đồng nhất. Các lập trình viên thực hiện điều này để định dạng, loại bỏ các ký tự không mong muốn, hoặc đơn giản hóa trước khi xử lý.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:58.291421-07:00
description: "C\xE1ch th\u1EE9c: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n c\xF3\
  \ m\u1ED9t chu\u1ED7i \u0111\u01B0\u1EE3c bao b\u1EDFi d\u1EA5u ngo\u1EB7c k\xE9\
  p, nh\u01B0 `\"\\\"Hello, World!\\\"\"` v\xE0 b\u1EA1n mu\u1ED1n v\u0103n b\u1EA3\
  n nguy\xEAn b\u1EA3n, kh\xF4ng d\u1EA5u ngo\u1EB7c. D\u01B0\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:37.137199-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n c\xF3 m\u1ED9t chu\u1ED7\
  i \u0111\u01B0\u1EE3c bao b\u1EDFi d\u1EA5u ngo\u1EB7c k\xE9p, nh\u01B0 `\"\\\"\
  Hello, World!\\\"\"` v\xE0 b\u1EA1n mu\u1ED1n v\u0103n b\u1EA3n nguy\xEAn b\u1EA3\
  n, kh\xF4ng d\u1EA5u ngo\u1EB7c."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cách thức:
Hãy tưởng tượng bạn có một chuỗi được bao bởi dấu ngoặc kép, như `"\"Hello, World!\""` và bạn muốn văn bản nguyên bản, không dấu ngoặc. Dưới đây là một đoạn mã JavaScript nhanh để giải phóng chuỗi của bạn khỏi những "xiềng xích" dấu ngoặc đó:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Kết quả: Hello, World!
```

Và nếu bạn đang xử lý dấu ngoặc đơn? Chỉ cần điều chỉnh biểu thức chính quy một chút:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Kết quả: Hello, World!
```

Hoặc nếu chuỗi của bạn là sự kết hợp của cả hai? Không vấn đề:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Kết quả: 'Hello, World!'
```

## Sâu hơn nữa
Trước khi JSON trở nên phổ biến, việc thoát khỏi dấu ngoặc là một cao nguyên hoang dã của dấu gạch chéo ngược và những thủ thuật đặc biệt. Các ngôn ngữ lập trình sớm không luôn luôn hợp tác tốt với dấu ngoặc, điều này có nghĩa là rất nhiều thao tác chuỗi thủ công. Bây giờ, với các định dạng dữ liệu được chuẩn hóa, việc loại bỏ dấu ngoặc thường là về việc làm sạch đầu vào trước khi chúng được xử lý như JSON hoặc lưu trữ văn bản mà không có xung đột định dạng.

Các phương án thay thế cho `.replace()`? Chắc chắn! Bạn có thể tách và kết hợp một chuỗi dựa trên dấu ngoặc, sử dụng slice nếu bạn chắc chắn về vị trí của những dấu ngoặc, hoặc thậm chí là sử dụng biểu thức chính quy để kéo ra văn bản cần thiết. Tất cả phụ thuộc vào bối cảnh.

Nhưng đừng quên về các trường hợp ngoại lệ: dấu ngoặc trong dấu ngoặc, dấu ngoặc được thoát, và kí tự quốc tế. Hãy coi chuỗi của bạn như một mảnh đất của các ngoại lệ tiềm ẩn, và tiến hành cẩn thận. Các trình thực thi JavaScript hiện đại được tối ưu hóa để xử lý các thao tác biểu thức chính quy một cách hiệu quả, vì vậy chúng thường là phương án đi đến, nhưng luôn đáng để kiểm tra hiệu suất cho các nhiệm vụ xử lý dữ liệu nặng.

## Xem thêm
Đào sâu hơn vào việc thao tác chuỗi và biểu thức chính quy:

- Mạng Lưới Nhà Phát Triển Mozilla về String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 để thử nghiệm các mẫu biểu thức chính quy của bạn: https://regex101.com/
- JSON.org để hiểu tại sao chúng ta phải xử lý nhiều dấu ngoặc trong phát triển web hiện đại: http://json.org/

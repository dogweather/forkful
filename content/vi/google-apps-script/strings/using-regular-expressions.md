---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:11.492701-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p\
  \ k\xFD t\u1EF1 trong chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa ho\u1EB7c\u2026"
lastmod: '2024-03-11T00:14:09.251994-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p\
  \ k\xFD t\u1EF1 trong chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa ho\u1EB7c\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Biểu thức chính quy (regex) là những mẫu được sử dụng để khớp các kết hợp ký tự trong chuỗi. Các lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa hoặc thao tác với văn bản và dữ liệu, khiến chúng trở nên không thể thiếu cho các nhiệm vụ khớp mẫu và phân tích dữ liệu.

## Làm thế nào:

Sử dụng biểu thức chính quy trong Google Apps Script khá đơn giản nhờ vào cú pháp dựa trên JavaScript. Dưới đây là cách bạn có thể tích hợp regex vào các kịch bản của mình cho các nhiệm vụ phổ biến như tìm kiếm và xác thực dữ liệu.

### Tìm kiếm Chuỗi

Giả sử bạn muốn tìm xem một chuỗi có chứa một mẫu cụ thể, như một địa chỉ email. Dưới đây là một ví dụ đơn giản:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Tìm thấy: " + found[0]);
  } else {
    Logger.log("Không tìm thấy email.");
  }
}

// Cách sử dụng mẫu
findEmailInText("Liên hệ với chúng tôi qua info@example.com.");
```

### Xác Thực Dữ Liệu

Biểu thức chính quy tỏa sáng trong việc xác thực dữ liệu. Dưới đây là một hàm kiểm tra một chuỗi đầu vào để kiểm tra xem nó có tuân thủ một chính sách mật khẩu đơn giản không (ít nhất một chữ cái viết hoa, một chữ cái viết thường và tối thiểu 8 ký tự).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Đầu ra mẫu
Logger.log(validatePassword("Str0ngPass")); // Đầu ra: true
Logger.log(validatePassword("weak"));       // Đầu ra: false
```

## Thảo Luận Sâu Hơn

Biểu thức chính quy trong Google Apps Script được kế thừa từ JavaScript, được chuẩn hóa lần đầu trong thông số kỹ thuật ngôn ngữ ECMAScript vào tháng 6 năm 1997. Mặc dù mạnh mẽ, chúng đôi khi có thể dẫn đến mã lệnh gây nhầm lẫn và khó duy trì, đặc biệt là khi sử dụng quá mức hoặc sử dụng cho các nhiệm vụ khớp mô hình phức tạp có thể được giải quyết hiệu quả hơn thông qua các phương pháp phân tích khác.

Ví dụ, trong khi bạn có thể sử dụng regex để phân tích HTML hoặc XML trong một tình huống khẩn cấp, việc làm này thường không được khuyến khích do cấu trúc lồng nhau và phức tạp của những tài liệu này. Thay vào đó, các công cụ được thiết kế riêng để phân tích những cấu trúc như vậy, như các bộ phân tích DOM cho HTML, là đáng tin cậy và dễ đọc hơn.

Hơn nữa, các nhà phát triển Google Apps Script nên lưu ý về các vấn đề hiệu suất tiềm ẩn khi sử dụng các mẫu regex phức tạp trong các nhiệm vụ thao tác văn bản quy mô lớn, vì việc xử lý regex có thể ngốn nhiều CPU. Trong những trường hợp như vậy, việc chia tác vụ thành các nhiệm vụ phụ đơn giản hơn hoặc sử dụng các hàm thao tác chuỗi có sẵn có thể cung cấp một sự cân bằng tốt hơn về hiệu suất và khả năng duy trì.

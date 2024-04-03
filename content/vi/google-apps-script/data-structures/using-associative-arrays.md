---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:43.394982-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, b\u1EA1n t\u1EA1o\
  \ v\xE0 thao t\xE1c m\u1EA3ng li\xEAn k\u1EBFt (\u0111\u1ED1i t\u01B0\u1EE3ng) s\u1EED\
  \ d\u1EE5ng d\u1EA5u ngo\u1EB7c nh\u1ECDn `{}`, \u0111\u1ECBnh ngh\u0129a c\xE1\
  c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB b\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.030110-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, b\u1EA1n t\u1EA1o v\xE0 thao t\xE1c m\u1EA3ng\
  \ li\xEAn k\u1EBFt (\u0111\u1ED1i t\u01B0\u1EE3ng) s\u1EED d\u1EE5ng d\u1EA5u ngo\u1EB7\
  c nh\u1ECDn `{}`, \u0111\u1ECBnh ngh\u0129a c\xE1c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB\
  \ b\xEAn trong."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Trong Google Apps Script, bạn tạo và thao tác mảng liên kết (đối tượng) sử dụng dấu ngoặc nhọn `{}`, định nghĩa các cặp khóa-giá trị bên trong. Khóa là các định danh duy nhất, và giá trị có thể là bất cứ thứ gì từ chuỗi và số đến các đối tượng phức tạp hơn hoặc các chức năng. Đây là một ví dụ cơ bản:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Truy cập giá trị
  Logger.log(user.name); // Đầu ra: John Doe
  Logger.log(user["email"]); // Đầu ra: johndoe@example.com

  // Thêm cặp khóa-giá trị mới
  user.title = "Nhà phát triển phần mềm";
  user["country"] = "Mỹ";

  Logger.log(user.title); // Đầu ra: Nhà phát triển phần mềm

  // Lặp qua các cặp khóa-giá trị
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Dữ liệu đầu ra cho phần lặp có thể trông như thế này:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Nhà phát triển phần mềm
country: Mỹ
```

Chú ý cách bạn có thể sử dụng cả ký hiệu dấu chấm và dấu ngoặc vuông để truy cập và thiết lập thuộc tính. Ký hiệu dấu ngoặc vuông đặc biệt hữu ích khi làm việc với các khóa được xác định động hoặc bao gồm các ký tự không được phép trong các định danh.

## Tìm hiểu sâu
Mảng liên kết dưới dạng đối tượng đã trở thành một nền tảng của JavaScript, và người kế thừa Google Apps Script, phản ánh cơ chế kế thừa dựa trên nguyên mẫu của nó. Không giống như các ngôn ngữ có mảng liên kết truyền thống hoặc từ điển (ví dụ: dict của Python), đối tượng Google Apps Script cung cấp một phương tiện linh hoạt và mạnh mẽ để cấu trúc dữ liệu, được hưởng lợi từ bản chất động của JavaScript.

Tuy nhiên, quan trọng là phải lưu ý rằng bản quy định ECMAScript 2015 đã giới thiệu đối tượng `Map` và `Set`, cung cấp một cách xử lý bộ sưu tập liên kết một cách trực tiếp hơn với một số lợi ích so với đối tượng, như duy trì thứ tự chèn và hiệu suất tốt hơn cho các bộ dữ liệu lớn. Mặc dù Google Apps Script cũng hỗ trợ chúng, nhưng việc chọn sử dụng đối tượng hay các cấu trúc `Map`/`Set` mới hơn phụ thuộc vào nhu cầu cụ thể và các xem xét về hiệu suất. Đối với hầu hết các nhiệm vụ mảng liên kết, triển khai dựa trên đối tượng truyền thống cung cấp một cách tiếp cận quen thuộc và linh hoạt, nhưng việc xem xét các lựa chọn mới hơn là điều khuyến khích khi độ phức tạp của kịch bản của bạn tăng lên.

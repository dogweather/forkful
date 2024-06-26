---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:10.643330-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1ED1i t\u01B0\u1EE3ng `String` c\u1EE7\
  a Arduino c\xF3 ph\u01B0\u01A1ng th\u1EE9c `toLowerCase()` ti\u1EC7n l\u1EE3i. G\u1ECD\
  i n\xF3 tr\xEAn chu\u1ED7i c\u1EE7a b\u1EA1n v\xE0 ngay l\u1EADp t\u1EE9c, n\xF3\
  \ s\u1EBD \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng."
lastmod: '2024-03-13T22:44:36.973724-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ED1i t\u01B0\u1EE3ng `String` c\u1EE7a Arduino c\xF3 ph\u01B0\u01A1\
  ng th\u1EE9c `toLowerCase()` ti\u1EC7n l\u1EE3i."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Làm thế nào:
Đối tượng `String` của Arduino có phương thức `toLowerCase()` tiện lợi. Gọi nó trên chuỗi của bạn và ngay lập tức, nó sẽ ở dạng chữ thường.

```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Hello, World!";
  message.toLowerCase();
  Serial.println(message);  // Đầu ra: hello, world!
}

void loop() {
  // Không có gì để làm ở đây.
}
```
Khởi động Serial Monitor của bạn, và bạn sẽ thấy "hello, world!" được in ra.

## Tìm hiểu sâu thêm
Trong quá khứ, việc xử lý văn bản thường liên quan đến việc tính toán cho cả chữ hoa và chữ thường. Việc nhập liệu, tìm kiếm và sắp xếp thường bỏ qua việc phân biệt chữ hoa chữ thường để giảm lỗi người dùng và tăng tính mạnh mẽ. Trong các ngôn ngữ khác, như C, bạn sẽ lặp qua từng ký tự và chuyển đổi chúng một cách riêng lẻ sử dụng các hàm thư viện chuẩn. Trong thế giới Arduino, các đối tượng `String` gói gọn chức năng này cho dễ sử dụng.

Có lựa chọn khác không? Chắc chắn rồi. Bạn có thể sử dụng `toLowerCase()` cho một mảng `char`, nhưng bạn sẽ phải đi qua từng ký tự và chuyển đổi chúng bằng `tolower()` từ `<ctype.h>`. Nếu bạn quan tâm đến bộ nhớ và hiệu suất, hãy xem xét sử dụng mảng ký tự thay vì các đối tượng `String` và kiểm soát với logic chuyển đổi chữ thường tự tạo của bạn.

## Xem thêm
- Trang tham khảo `String` của Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Thư viện `<cctype>` của C++ cho các hoạt động với ký tự: http://www.cplusplus.com/reference/cctype/
- Để hiểu cách hoạt động của so sánh chuỗi và tại sao bỏ qua trường hợp chữ hoa có thể quan trọng, hãy kiểm tra: https://en.wikipedia.org/wiki/String_(computer_science)#Comparison

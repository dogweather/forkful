---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:10.643330-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\
  \u1EDDng thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1i in hoa trong\
  \ v\u0103n b\u1EA3n th\xE0nh c\xE1c ch\u1EEF c\xE1i in th\u01B0\u1EDDng t\u01B0\u01A1\
  ng \u1EE9ng c\u1EE7a ch\xFAng. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.973724-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDD\
  ng thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1i in hoa trong v\u0103\
  n b\u1EA3n th\xE0nh c\xE1c ch\u1EEF c\xE1i in th\u01B0\u1EDDng t\u01B0\u01A1ng \u1EE9\
  ng c\u1EE7a ch\xFAng. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cái gì & Tại sao?
Chuyển đổi một chuỗi sang chữ thường thay đổi tất cả các chữ cái in hoa trong văn bản thành các chữ cái in thường tương ứng của chúng. Các lập trình viên thực hiện điều này để đảm bảo tính nhất quán, đặc biệt là khi so sánh các chuỗi hoặc chuẩn hóa dữ liệu đầu vào.

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

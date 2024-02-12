---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:10.643330-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

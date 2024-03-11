---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:23.580348-07:00
description: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 x\xE1c \u0111\u1ECBnh xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1\
  c minh \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, c\u0103n\u2026"
lastmod: '2024-03-11T00:14:10.275124-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 x\xE1c \u0111\u1ECBnh xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1\
  c minh \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, c\u0103n\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tìm độ dài của một chuỗi nghĩa là xác định xem nó chứa bao nhiêu ký tự. Lập trình viên thực hiện điều này để xác minh đầu vào, lặp qua các ký tự, căn chỉnh văn bản, hoặc quản lý dữ liệu một cách động.

## Làm thế nào:
```Arduino
void setup() {
  Serial.begin(9600); // Bắt đầu giao tiếp nối tiếp
  String myString = "Hello, Arduino!"; // Chuỗi của bạn ở đây
  int stringLength = myString.length(); // Tìm độ dài của chuỗi
  Serial.print("Độ dài của chuỗi là: ");
  Serial.println(stringLength); // Xuất ra độ dài
}

void loop() {
  // Không có gì phải làm ở đây.
}
```
Kết quả Mẫu:
```
Độ dài của chuỗi là: 15
```

## Sâu hơn
Ngày xưa, các lập trình viên C sử dụng hàm `strlen()` từ `<string.h>`, đếm các ký tự cho đến khi gặp ký tự kết thúc chuỗi (null-terminator). Trong thế giới của Arduino, lớp `String` làm cho cuộc sống dễ dàng hơn với phương thức `length()` tích hợp sẵn của nó. Nhưng nhớ rằng, việc sử dụng đối tượng `String` có thể làm mảnh vỡ bộ nhớ hạn chế của thiết bị theo thời gian. Một lựa chọn khác? Sử dụng mảng ký tự (chuỗi kiểu C), đó là thân thiện với bộ nhớ hơn nhưng khó xử lý hơn.

Đối với các dự án lớn hơn, luôn cần xem xét quản lý bộ nhớ. Với phương thức `length()`, không cần tính toán thêm - đối tượng `String` tự giữ kích thước của nó. Trên thực tế, `length()` là một việc kiểm tra nhanh, không phải là việc đếm ký tự. Điều đó hiệu quả! Nhưng, nếu bạn gặp vấn đề về bộ nhớ, hãy quay trở lại căn bản với mảng ký tự và việc tính toán độ dài thủ công, giống như ngày xưa với hàm `strlen()`.

## Xem thêm
- Tham khảo `String` của Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Hàm `strlen()` cho chuỗi kiểu C của Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Thảo luận về `String` vs. mảng ký tự trong Arduino: https://forum.arduino.cc/t/string-vs-char-array/678207

---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- /vi/arduino/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:23.580348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

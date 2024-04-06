---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:23.580348-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ng\xE0y x\u01B0a, c\xE1c l\u1EADp tr\xECnh vi\xEA\
  n C s\u1EED d\u1EE5ng h\xE0m `strlen()` t\u1EEB `<string.h>`, \u0111\u1EBFm c\xE1\
  c k\xFD t\u1EF1 cho \u0111\u1EBFn khi g\u1EB7p k\xFD t\u1EF1 k\u1EBFt th\xFAc chu\u1ED7\
  i (null-terminator).\u2026"
lastmod: '2024-04-05T22:50:51.290458-06:00'
model: gpt-4-0125-preview
summary: "Ng\xE0y x\u01B0a, c\xE1c l\u1EADp tr\xECnh vi\xEAn C s\u1EED d\u1EE5ng h\xE0\
  m `strlen()` t\u1EEB `<string.h>`, \u0111\u1EBFm c\xE1c k\xFD t\u1EF1 cho \u0111\
  \u1EBFn khi g\u1EB7p k\xFD t\u1EF1 k\u1EBFt th\xFAc chu\u1ED7i (null-terminator)."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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

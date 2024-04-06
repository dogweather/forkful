---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:51.764009-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y c\xF9ng th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\u1ED1i chu\u1ED7i! T\u1EA5t c\u1EA3 s\u1EBD \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7\
  n trong h\xE0m setup v\xEC ch\xFAng ta ch\u1EC9 mu\u1ED1n xem qua m\u1ED9t l\u1EA7\
  n\u2014kh\xF4ng c\u1EA7n v\xF2ng l\u1EB7p l\u1EB7p\u2026"
lastmod: '2024-04-05T22:37:45.691965-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y c\xF9ng th\u1EF1c hi\u1EC7n vi\u1EC7c n\u1ED1i chu\u1ED7i!."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
Hãy cùng thực hiện việc nối chuỗi! Tất cả sẽ được thực hiện trong hàm setup vì chúng ta chỉ muốn xem qua một lần—không cần vòng lặp lặp lại.

```arduino
void setup() {
  // Bắt đầu giao tiếp nối tiếp
  Serial.begin(9600);

  // Tạo hai chuỗi
  String greeting = "Xin chào, ";
  String name = "Arduino!";

  // Nối chúng lại
  String combined = greeting + name;

  // In kết quả
  Serial.println(combined); 
}
void loop() {
  // Không có gì để lặp lại ở đây
}
```

Bạn chạy chương trình, và kết quả chờ đợi bạn trong Serial Monitor:

```
Xin chào, Arduino!
```

## Thảo luận sâu
Nối chuỗi đã có từ rất lâu trong lập trình—tồn tại ngay từ khi các ngôn ngữ lập trình đầu tiên hình thành. Trong Arduino, bạn có thể sử dụng toán tử `+` như chúng tôi đã làm, hoặc `+=` để thêm một chuỗi vào một chuỗi đã có. Phía sau, các toán tử này thực chất đang gọi các hàm xử lý việc cấp phát bộ nhớ và sao chép các ký tự một cách hiệu quả.

Tại sao không luôn luôn nối chuỗi? Nếu bạn đang làm việc với các bộ vi điều khiển nhỏ và thực hiện nhiều thao tác nối chuỗi, bạn có thể gặp phải vấn đề về bộ nhớ—bởi vì mỗi lần bạn kết hợp, bạn tạo ra một chuỗi mới, tiêu thụ thêm bộ nhớ. Đối với việc thao tác chuỗi nặng nề, mọi người đôi khi chuyển sang sử dụng mảng ký tự (phong cách C cổ điển) để tiết kiệm không gian và tránh ảnh hưởng tiềm ẩn đến hiệu suất.

Ngoài ra, hãy kiểm tra các hàm chuỗi như `concat()`, có thể thêm không chỉ chuỗi mà còn các kiểu dữ liệu khác vào một chuỗi đã có.

## Xem Thêm
Muốn tìm hiểu thêm? Đây là nơi để đào sâu hơn:
- Tham khảo Chuỗi Arduino: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Quản lý Bộ Nhớ trong Arduino: [learn.adafruit.com/memories-of-an-arduino](https://learn.adafruit.com/memories-of-an-arduino)
- Các điều không tốt của Chuỗi Arduino: [majenko.co.uk/blog/evils-arduino-strings](https://majenko.co.uk/blog/evils-arduino-strings)

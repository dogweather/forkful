---
title:                "Nối chuỗi ký tự"
aliases: - /vi/arduino/concatenating-strings.md
date:                  2024-01-28T21:56:51.764009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nối chuỗi là việc ghép chúng lại với nhau từ đầu đến cuối để tạo ra một chuỗi mới. Lập trình viên làm việc này để kết hợp các thông điệp, xây dựng lệnh, hoặc chỉ đơn giản là hiển thị thông tin một cách gọn gàng.

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

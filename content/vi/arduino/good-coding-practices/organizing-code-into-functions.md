---
title:                "Sắp xếp mã thành các hàm"
aliases: - /vi/arduino/organizing-code-into-functions.md
date:                  2024-01-28T22:03:23.404633-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức mã lệnh thành các hàm nghĩa là phân chia mã lệnh của bạn thành các phần có thể tái sử dụng, mỗi phần thực hiện một công việc cụ thể. Lập trình viên làm điều này để làm cho mã lệnh dễ đọc, dễ gỡ lỗi, và tái sử dụng hơn. Nó giống như việc sắp xếp Lego vào các hộp - nó giúp bạn tránh phải lục lọi qua một đống hỗn loạn mỗi lần bạn muốn xây dựng thứ gì đó.

## Cách thực hiện:
Hãy tưởng tượng bạn muốn nhấp nháy một đèn LED. Không có hàm, `vòng lặp` của bạn sẽ là một đống lộn xộn. Với hàm, nó gọn gàng. Đây là cách:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Nhấp nháy đèn LED mỗi 500ms
}

// Hàm để nhấp nháy đèn LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Kết quả mẫu: Đèn LED của bạn nhấp nháy vui vẻ, và mục đích của mã lệnh rõ ràng ngay từ cái nhìn đầu tiên.

## Sâu hơn
Trước khi có hàm, lập trình giống như một chuyến đi trên con đường tuyến tính; bạn thấy mỗi ổ gà từ đầu đến cuối. Sau khi có hàm, nó giống như việc nhảy các chuyến bay - bạn bỏ qua những phần quan trọng. Lịch sử, các phụ trình (hàm sớm) đã là một cuộc cách mạng trong lập trình, cho phép lập trình viên tránh lặp lại bản thân – đó là nguyên tắc DRY, Đừng Lặp Lại Bản Thân. Các phương án thay thế cho hàm có thể bao gồm macro hoặc sử dụng các lớp cho lập trình hướng đối tượng (OOP). Điều cần quan tâm? Khi bạn định nghĩa một hàm, bạn đang cung cấp cho trình biên dịch một bản vẽ để thực hiện một nhiệm vụ. Với Arduino, bạn thường định nghĩa các hàm void hành động như các lệnh đơn giản cho một vi điều khiển, nhưng hàm cũng có thể trả về các giá trị, làm cho chúng đa dạng hơn.

## Xem thêm
Để tìm hiểu thêm về hàm, hãy duyệt qua những tài liệu sau:

- Tài liệu tham khảo hàm chính thức của Arduino: https://www.arduino.cc/reference/en/language/functions/
- Tìm hiểu thêm về nguyên tắc DRY: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- Đọc lại lịch sử về các phụ trình: https://en.wikipedia.org/wiki/Subroutine

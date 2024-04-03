---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:23.404633-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng\
  \ b\u1EA1n mu\u1ED1n nh\u1EA5p nh\xE1y m\u1ED9t \u0111\xE8n LED. Kh\xF4ng c\xF3\
  \ h\xE0m, `v\xF2ng l\u1EB7p` c\u1EE7a b\u1EA1n s\u1EBD l\xE0 m\u1ED9t \u0111\u1ED1\
  ng l\u1ED9n x\u1ED9n. V\u1EDBi h\xE0m, n\xF3 g\u1ECDn g\xE0ng. \u0110\xE2y l\xE0\
  \ c\xE1ch."
lastmod: '2024-03-13T22:44:36.996066-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n mu\u1ED1n nh\u1EA5p nh\xE1\
  y m\u1ED9t \u0111\xE8n LED."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:23.404633-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m ngh\u0129a\
  \ l\xE0 ph\xE2n chia m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1n th\xE0nh c\xE1c ph\u1EA7\
  n c\xF3 th\u1EC3 t\xE1i s\u1EED d\u1EE5ng, m\u1ED7i ph\u1EA7n th\u1EF1c hi\u1EC7\
  n m\u1ED9t c\xF4ng vi\u1EC7c c\u1EE5 th\u1EC3. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-11T00:14:10.293244-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m ngh\u0129a l\xE0\
  \ ph\xE2n chia m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1n th\xE0nh c\xE1c ph\u1EA7n c\xF3\
  \ th\u1EC3 t\xE1i s\u1EED d\u1EE5ng, m\u1ED7i ph\u1EA7n th\u1EF1c hi\u1EC7n m\u1ED9\
  t c\xF4ng vi\u1EC7c c\u1EE5 th\u1EC3. L\u1EADp tr\xECnh\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
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

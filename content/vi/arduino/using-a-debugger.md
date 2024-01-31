---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:29.661836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Debugger là một công cụ giúp bạn tìm và sửa lỗi trong mã của mình bằng cách cho phép bạn dừng, khám phá và tìm hiểu điều gì đang thực sự diễn ra bên trong. Lập trình viên sử dụng debugger để bước qua từng dòng mã của họ, kiểm tra các biến và hiểu nơi nào có thể đang xảy ra sự cố.

## Làm thế nào:

Với Arduino IDE, bạn có thể sử dụng in ra Serial để debug, nhưng nó giống như sử dụng một chiếc đèn pin để khám phá một hang động. Để debug thực sự, bạn có thể muốn nâng cấp trò chơi của mình với thứ gì đó như debugger Atmel-ICE, cái mà tích hợp với môi trường Arduino. Dưới đây là một ví dụ về pseudo-debugging sử dụng Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Giá trị cảm biến: ");
  Serial.println(sensorValue);
  // Tưởng tượng bạn đang mong đợi 512 ở đây, nhưng nhận được 0.
  // Đã đến lúc kiểm tra kết nối cảm biến
  delay(1000); // Chờ một giây trước khi đọc lại
}
```
Chạy điều này với Serial Monitor mở, và bạn sẽ thấy những gì cảm biến của mình phát ra theo thời gian thực.

## Sâu hơn nữa

Trước khi có debugger, đó là thế giới của lệnh in ra – bạn chỉ có thể đoán xem điều gì đang xảy ra bằng cách in tất cả ra. Debugging bằng việc in ra vẫn phổ biến, đặc biệt là trong các môi trường đơn giản hoặc trên phần cứng hạn chế như Arduino.

Các lựa chọn thay thế cho các bộ mô phỏng trong mạch như Atmel-ICE bao gồm các công cụ debug phần mềm như `avr-gdb`. Bạn có thể kết hợp nó với `avarice` để tạo ra một cầu nối giữa GDB và phần cứng của bạn, điều này rất tiện lợi cho việc debug nâng cao ngay trên chip.

Sử dụng một debugger, bạn có thể đặt các điểm dừng để dừng việc thực thi tại một số điểm nhất định. Bạn có thể bước qua mã của mình từng dòng một, kiểm tra bộ nhớ, các bộ đăng ký và biến. Điều này cho phép bạn xác định vấn đề thay vì mò mẫm trong bóng tối. Khi triển khai một debugger, hãy đảm bảo môi trường của bạn được thiết lập đúng cách - các phiên bản không khớp hoặc công cụ cấu hình kém có thể dẫn đến sự thất vọng.

## Xem thêm

Sẵn sàng đi sâu hơn? Khám phá những nội dung này:
- Hướng dẫn debug Arduino tại [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- Sổ tay tham khảo AVR Libc để thiết lập avr-gdb: [Trang chủ AVR Libc](http://www.nongnu.org/avr-libc/)

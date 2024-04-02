---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.002959-07:00
description: "In \u0111\u1EA7u ra g\u1EE1 l\u1ED7i v\xE0o m\xE0n h\xECnh n\u1ED1i\
  \ ti\u1EBFp l\xE0 m\u1ED9t c\xE1ch \u0111\u1EC3 nh\xECn v\xE0o t\xE2m tr\xED c\u1EE7\
  a Arduino. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ theo d\xF5i c\xE1c l\u1ED7i nh\u1ECF, ki\u1EC3m th\u1EED gi\u1EA3\u2026"
lastmod: '2024-03-13T22:44:36.992262-06:00'
model: gpt-4-0125-preview
summary: "In \u0111\u1EA7u ra g\u1EE1 l\u1ED7i v\xE0o m\xE0n h\xECnh n\u1ED1i ti\u1EBF\
  p l\xE0 m\u1ED9t c\xE1ch \u0111\u1EC3 nh\xECn v\xE0o t\xE2m tr\xED c\u1EE7a Arduino.\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\
  \ d\xF5i c\xE1c l\u1ED7i nh\u1ECF, ki\u1EC3m th\u1EED gi\u1EA3\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Gì và Tại Sao?

In đầu ra gỡ lỗi vào màn hình nối tiếp là một cách để nhìn vào tâm trí của Arduino. Các lập trình viên làm điều này để theo dõi các lỗi nhỏ, kiểm thử giả định và giám sát dữ liệu thời gian thực mà không cần sử dụng các kỹ thuật gỡ lỗi phức tạp.

## Cách thực hiện:

Cùng vào vấn đề chính. Giả sử bạn muốn in "Hello, world!" mỗi giây. Dưới đây là đoạn mã:

```Arduino
void setup() {
  Serial.begin(9600);  // Bắt đầu truyền thông nối tiếp
}

void loop() {
  Serial.println("Hello, world!");  // In thông điệp
  delay(1000);  // Đợi một giây
}
```

Bật Màn hình Nối tiếp trong Arduino IDE và xem các từ rơi xuống như đồng hồ. Đầu ra mẫu:

```
Hello, world!
Hello, world!
Hello, world!
...
```

## Đi sâu hơn

Trước khi `Serial` trở thành đồng minh đáng tin cậy của chúng ta, mọi người sử dụng đèn LED nhấp nháy để giao tiếp - kỷ nguyên đồ đá của việc gỡ lỗi. Sau đó, phần cứng gỡ lỗi nghiêm túc xuất hiện, nhưng nó đắt đỏ. `Serial.print()` và các hàm liên quan giờ đây cho phép chúng ta gửi văn bản lên màn hình với tốc độ chóng mặt, rẻ như bèo.

Các lựa chọn khác? Có, bạn có LCD, lưu trữ vào thẻ SD, thậm chí là Bluetooth cho những người không thích dây. Mỗi phương pháp đều có những tính năng đặc trưng; `Serial` chỉ đơn giản, trực tiếp, luôn có mặt.

Bên trong, `Serial.print()` chuyển đổi dữ liệu của bạn thành các byte di chuyển dọc theo USB đến máy tính của bạn. Điều này xảy ra qua các cổng nối tiếp phần cứng (UART) hoặc được mô phỏng bằng phần mềm (SoftSerial). Nó đáng tin cậy, nhưng nếu chiếm dụng cổng với quá nhiều dữ liệu có thể làm tắc nghẽn dòng chảy của chương trình của bạn, vì vậy hãy rắc các lệnh in nối tiếp như bạn đang ướp thịt, không phải làm đầy một bát súp.

## Xem Thêm

Dành cho những người muốn tìm hiểu thêm:

- Hướng dẫn của Arduino về `Serial`: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Về khoa học đằng sau giao tiếp nối tiếp: [Giao tiếp UART](https://www.sparkfun.com/tutorials/215)

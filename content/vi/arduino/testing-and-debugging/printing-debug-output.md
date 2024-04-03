---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.002959-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C\xF9ng v\xE0o v\u1EA5n \u0111\u1EC1 ch\xED\
  nh. Gi\u1EA3 s\u1EED b\u1EA1n mu\u1ED1n in \"Hello, world!\" m\u1ED7i gi\xE2y. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 \u0111o\u1EA1n m\xE3."
lastmod: '2024-03-13T22:44:36.992262-06:00'
model: gpt-4-0125-preview
summary: "C\xF9ng v\xE0o v\u1EA5n \u0111\u1EC1 ch\xEDnh."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:21.080939-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 l\u1EA5y d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c truy\u1EC1n \u0111\u1EBF\
  n m\u1ED9t ch\u01B0\u01A1ng tr\xECnh khi b\u1EA1n kh\u1EDFi \u0111\u1ED9ng n\xF3\
  \ t\u1EEB terminal ho\u1EB7c d\u1EA5u nh\u1EAFc l\u1EC7nh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:37.009667-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 l\u1EA5y d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c truy\u1EC1n \u0111\u1EBF\
  n m\u1ED9t ch\u01B0\u01A1ng tr\xECnh khi b\u1EA1n kh\u1EDFi \u0111\u1ED9ng n\xF3\
  \ t\u1EEB terminal ho\u1EB7c d\u1EA5u nh\u1EAFc l\u1EC7nh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Cái gì và Tại sao?
Đọc các đối số dòng lệnh có nghĩa là lấy dữ liệu được truyền đến một chương trình khi bạn khởi động nó từ terminal hoặc dấu nhắc lệnh. Các lập trình viên sử dụng đối số để tùy chỉnh hành vi của chương trình mà không cần thay đổi mã.

## Làm thế nào:
Arduino không xử lý đối số dòng lệnh như các môi trường lập trình truyền thống, bởi vì bản vẽ (sketches) được tải lên microcontrollers mà không có dấu nhắc lệnh OS có thể truy cập được. Nhưng bạn có thể mô phỏng tính năng này bằng cách sử dụng giao tiếp nối tiếp. Đây là cách thức:

```arduino
void setup() {
  // Khởi tạo giao tiếp nối tiếp với tốc độ 9600 bit mỗi giây:
  Serial.begin(9600);
}

void loop() {
  // Kiểm tra nếu có dữ liệu sẵn sàng để đọc.
  if (Serial.available() > 0) {
    // Đọc các byte đến khi nhận được một dấu xuống dòng mới.
    String receivedData = Serial.readStringUntil('\n');
    // Phản hồi lại các đối số nhận được vào màn hình nối tiếp.
    Serial.print("Đã nhận: ");
    Serial.println(receivedData);
  }
}
```

Mẫu Đầu ra Màn hình Nối tiếp:
```
Đã nhận: argument1 argument2 argument3
```

## Sâu hơn
Các đối số dòng lệnh truyền thống hoạt động trong hệ điều hành đầy đủ (như Windows, Linux, hoặc macOS) chạy các chương trình. Bộ xử lý lệnh của OS truyền đối số cho các chương trình. Arduino không có điều này; đó là một microcontroller với một chương trình duy nhất chạy lặp đi lặp lại.

Giao tiếp nối tiếp là giải pháp thay thế của bạn. Nó giống như việc có cuộc trò chuyện với Arduino của bạn qua một đường dây dành riêng. Bạn gửi dữ liệu qua đường dây này, mà chương trình Arduino đọc là input khi nó sẵn lòng.

Trước khi Màn hình Nối tiếp trong Arduino IDE, các lập trình viên sử dụng công tắc vật lý hoặc jumper trên phần cứng để thay đổi hành vi. Giao tiếp nối tiếp là một bước tiến lớn, đơn giản hóa quy trình này rất nhiều.

Nhớ rằng, Arduino Uno và nhiều loại khác chỉ có một cổng nối tiếp chia sẻ với kết nối USB, nghĩa là bạn không thể nhận dữ liệu nối tiếp và tải lên một bản vẽ mới cùng một lúc. Các bo mạch Arduino tiên tiến hơn có thể có nhiều cổng nối tiếp, cho phép giao tiếp và tải lên bản vẽ cùng một lúc.

Các phương pháp thay thế cho giao tiếp nối tiếp để mô phỏng đối số dòng lệnh bao gồm:

- Mô-đun Bluetooth (cho giao tiếp không dây).
- Bàn phím hoặc nút bấm cho input.
- Lưu trữ đối số vào EEPROM (bộ nhớ không bay hơi) và đọc chúng khi khởi động.

Mỗi phương pháp có trường hợp sử dụng và độ phức tạp riêng, nhưng giao tiếp nối tiếp là đơn giản nhất cho việc lập mẫu nhanh và kiểm tra.

## Xem thêm
- Giao tiếp Nối tiếp Arduino: [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Đọc và Viết EEPROM Arduino: [Arduino - EEPROM](https://www.arduino.cc/en/Reference/EEPROM)

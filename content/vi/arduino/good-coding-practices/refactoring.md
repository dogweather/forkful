---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:54.229591-07:00
description: "L\xE0m th\u1EBF n\xE0o: Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t h\xE0\
  m tr\xEAn Arduino c\u1EE7a m\xECnh \u0111ang l\xE0m qu\xE1 nhi\u1EC1u vi\u1EC7c,\
  \ nh\u01B0 th\u1EBF n\xE0y."
lastmod: '2024-03-13T22:44:36.999990-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t h\xE0m tr\xEAn Arduino c\u1EE7\
  a m\xECnh \u0111ang l\xE0m qu\xE1 nhi\u1EC1u vi\u1EC7c, nh\u01B0 th\u1EBF n\xE0\
  y."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
Giả sử bạn có một hàm trên Arduino của mình đang làm quá nhiều việc, như thế này:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Một hàm đang làm quá nhiều việc
  handleEverything();
}

void handleEverything() {
  // Đọc dữ liệu cảm biến
  int sensorValue = analogRead(A0);
  // Xử lý dữ liệu cảm biến
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // In dữ liệu cảm biến
  Serial.println(sensorValue);
  delay(500);
}
```

Tái cấu trúc nó có thể trông giống như tách `handleEverything()` thành các hàm nhỏ hơn, tập trung hơn:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Sau khi tái cấu trúc, hàm `loop()` dễ đọc hơn và mỗi nhiệm vụ được xử lý bởi một hàm riêng biệt, làm cho code dễ quản lý hơn.

## Sâu hơn
Lịch sử, tái cấu trúc trở nên phổ biến với sự nổi lên của các phương pháp Agile và Phát triển Hướng theo Kiểm thử (TDD), dựa vào việc cải thiện liên tục code để thích nghi với nhu cầu thay đổi. Có nhiều công cụ và chiến lược tái cấu trúc - giống như kỹ thuật "Trích xuất Phương pháp" mà chúng ta đã sử dụng trong ví dụ Arduino của mình. Điều này thiết yếu khi bạn chuyển từ một nguyên mẫu nhanh sang một dự án ổn định, nơi mà đọc hiểu code và bảo trì trở nên quan trọng.

Khi tái cấu trúc, việc có một bộ kiểm thử tốt là quan trọng để đảm bảo rằng thay đổi không giới thiệu bất kỳ lỗi nào. Trong thế giới Arduino, kiểm thử tự động không luôn là điều đơn giản do phụ thuộc vào phần cứng, nhưng bạn vẫn có thể sử dụng kiểm thử đơn vị cho các phần logic thuần túy hoặc sử dụng bộ mô phỏng.

Các phương án thay thế cho tái cấu trúc bằng tay bao gồm sử dụng các công cụ tái cấu trúc chuyên dụng, giúp tự động hóa việc nhận dạng mùi code và đề xuất thay đổi. Tuy nhiên, các công cụ này thường thiếu sự tinh vi cho code vi điều khiển và có thể không có sẵn trong môi trường phát triển Arduino.

Cuối cùng, tái cấu trúc là một nghệ thuật cân bằng giữa việc cải thiện cấu trúc nội bộ của code chống lại rủi ro giới thiệu lỗi. Nó yêu cầu bạn suy nghĩ về chi tiết triển khai như sử dụng bộ nhớ và thời gian xử lý, đặc biệt do bản chất hạn chế tài nguyên của vi điều khiển.

## Xem thêm
Bạn có thể tìm hiểu sâu hơn về tái cấu trúc với cuốn sách kinh điển của Martin Fowler *Refactoring: Improving the Design of Existing Code*. Để có cái nhìn cận cảnh hơn về các thực hành cụ thể của Arduino, hãy kiểm tra các diễn đàn và cộng đồng phát triển Arduino:

- [Diễn đàn Arduino - Câu hỏi Lập trình](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Hãy nhớ, mục tiêu là code sạch, dễ hiểu mà bạn trong tương lai, và người khác, sẽ cảm ơn bạn. Hãy tiếp tục hack, và giữ nó gọn gàng!

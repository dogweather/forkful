---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:58.312429-07:00
description: "Ghi v\xE0o l\u1ED7i chu\u1EA9n (stderr) b\xE1o c\xE1o l\u1ED7i v\xE0\
  \ ch\u1EA9n \u0111o\xE1n ri\xEAng bi\u1EC7t t\u1EEB \u0111\u1EA7u ra chu\u1EA9n\
  \ (stdout). \u0110i\u1EC1u n\xE0y quan tr\u1ECDng cho vi\u1EC7c g\u1EE1 l\u1ED7\
  i v\xE0 l\u01B0u log, gi\xFAp c\xE1c nh\xE0 ph\xE1t\u2026"
lastmod: '2024-03-13T22:44:37.010955-06:00'
model: gpt-4-0125-preview
summary: "Ghi v\xE0o l\u1ED7i chu\u1EA9n (stderr) b\xE1o c\xE1o l\u1ED7i v\xE0 ch\u1EA9\
  n \u0111o\xE1n ri\xEAng bi\u1EC7t t\u1EEB \u0111\u1EA7u ra chu\u1EA9n (stdout)."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Cách thực hiện:
Arduino không hỗ trợ natively stderr, nhưng chúng ta có thể mô phỏng nó bằng cách ghi vào Serial. Hãy tưởng tượng một chương trình nhấp nháy LED có kiểm tra lỗi:

```Arduino
void setup() {
  Serial.begin(9600);
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  if(!digitalWriteCheck(LED_BUILTIN, HIGH)) {
    Serial.println("Lỗi: Không thể đặt LED ở trạng thái cao"); // Đây là "stderr" của chúng ta
  }
  delay(1000); // Chờ một giây
  if(!digitalWriteCheck(LED_BUILTIN, LOW)) {
    Serial.println("Lỗi: Không thể đặt LED ở trạng thái thấp"); // Đây là "stderr" của chúng ta
  }
  delay(1000); // Chờ một giây
}

bool digitalWriteCheck(int pin, int giá_trị) {
  // Giả sử hàm này kiểm tra xem digitalWrite có thành công không
  digitalWrite(pin, giá_trị);
  // Nếu thành công trả về true, chúng ta luôn thất bại trong ví dụ này
  return false;
}
```

Mẫu Đầu ra:
```
Lỗi: Không thể đặt LED ở trạng thái cao
Lỗi: Không thể đặt LED ở trạng thái thấp
```

## Sâu hơn
Lịch sử, stderr là một luồng chuẩn trong nhiều hệ điều hành, được giới thiệu bởi Unix. Trong Arduino, không có hệ điều hành, chúng ta tự tay xuất lỗi sử dụng Serial.print hoặc tương tự. Nếu bạn đang log vào máy tính, nhật ký có thể được chuyển hướng từ Serial sang một tập tin, tách biệt chúng ra khỏi stdout một cách hiệu quả. Người dùng nâng cao có thể sử dụng SoftwareSerial để mô phỏng stderr trên các cổng serial phần cứng khác nhau.

## Xem thêm
- Tài liệu chính thức của Arduino về Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Chuẩn Streams của Unix: https://en.wikipedia.org/wiki/Standard_streams
- Thư viện SoftwareSerial: https://www.arduino.cc/en/Reference/SoftwareSerial

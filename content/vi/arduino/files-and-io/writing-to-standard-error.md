---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/arduino/writing-to-standard-error.md
date:                  2024-01-28T22:13:58.312429-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Ghi vào lỗi chuẩn (stderr) báo cáo lỗi và chẩn đoán riêng biệt từ đầu ra chuẩn (stdout). Điều này quan trọng cho việc gỡ lỗi và lưu log, giúp các nhà phát triển tách biệt vấn đề mà không làm lẫn lộn thông điệp lỗi với đầu ra chương trình bình thường.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:32.107661-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Arduino kh\xF4ng \u0111i k\xE8m v\u1EDB\
  i m\u1ED9t th\u01B0 vi\u1EC7n ghi ch\xE9p s\u1EB5n c\xF3 nh\u01B0 m\u1ED9t s\u1ED1\
  \ m\xF4i tr\u01B0\u1EDDng kh\xE1c, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 th\u1EF1c\
  \ hi\u1EC7n vi\u1EC7c ghi ch\xE9p c\u01A1 b\u1EA3n v\xE0o b\u1EA3ng\u2026"
lastmod: '2024-03-13T22:44:36.997358-06:00'
model: gpt-4-0125-preview
summary: "Arduino kh\xF4ng \u0111i k\xE8m v\u1EDBi m\u1ED9t th\u01B0 vi\u1EC7n ghi\
  \ ch\xE9p s\u1EB5n c\xF3 nh\u01B0 m\u1ED9t s\u1ED1 m\xF4i tr\u01B0\u1EDDng kh\xE1\
  c, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 th\u1EF1c hi\u1EC7n vi\u1EC7c ghi ch\xE9p\
  \ c\u01A1 b\u1EA3n v\xE0o b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n Serial v\u1EDBi r\u1EA5\
  t \xEDt r\u1EAFc r\u1ED1i."
title: Ghi log
weight: 17
---

## Cách thực hiện:
Arduino không đi kèm với một thư viện ghi chép sẵn có như một số môi trường khác, nhưng bạn có thể thực hiện việc ghi chép cơ bản vào bảng điều khiển Serial với rất ít rắc rối. Dưới đây là một ví dụ nhanh để bạn bắt đầu:

```arduino
void setup() {
  // Bắt đầu giao tiếp serial với tốc độ baud đã cho
  Serial.begin(9600);

  // Chờ cổng serial kết nối - chỉ cần thiết trên một số bảng mạch
  while (!Serial) {
    ; // đợi cổng serial kết nối. Cần cho USB gốc
  }

  // Ghi một thông báo thông tin cho thấy quá trình thiết lập đã hoàn tất
  Serial.println("Quá trình thiết lập hoàn tất!");
}

void loop() {
  // Bộ logger đơn giản in ra thời gian hoạt động mỗi giây
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Thời gian hoạt động (ms): ");
    Serial.println(currentMillis);

    // Tại đây bạn cũng có thể thêm các nhật ký lỗi, cảnh báo, hoặc thông tin khác.
  }
  
  // Logic phần còn lại của chương trình của bạn ở đây...
}
```

Mẫu đầu ra Serial:
```
Quá trình thiết lập hoàn tất!
Thời gian hoạt động (ms): 1000
Thời gian hoạt động (ms): 2000
Thời gian hoạt động (ms): 3000
...
```

## Sâu hơn nữa:
Về lịch sử, việc ghi chép trên microcontrollers không được dễ dàng như trên một hệ điều hành đầy đủ. Nguyên liệu hạn chế có nghĩa là mỗi byte đều quan trọng, và các nhà phát triển cần phải cẩn thận không làm tắc nghẽn hệ thống. Với sự xuất hiện của các bảng mạch bộ xử lý mạnh mẽ và nền tảng Arduino đơn giản hóa quy trình, việc ghi chép đã trở nên dễ dàng hơn.

Mặc dù mã ở trên minh họa việc ghi chép thông qua giao diện Serial, nhưng các phương pháp khác bao gồm việc ghi vào thẻ SD, gửi dữ liệu qua mạng đến một máy chủ từ xa, hoặc thậm chí xuất ra một LCD nhỏ.

Việc triển khai một hệ thống ghi chép mang lại những cân nhắc như luân chuyển, mức độ nghiêm trọng (thông tin, gỡ lỗi, cảnh báo, lỗi) và ảnh hưởng đến hiệu suất. Trên một Arduino, bạn có thể cần phải chú ý đến ràng buộc về bộ nhớ khi ghi chép các cấu trúc dữ liệu phức tạp. Đối với ghi chép từ xa, bảo mật của các nhật ký được truyền đi cũng là một mối quan tâm.

Các giải pháp tinh vi hơn như Syslog, một tiêu chuẩn ghi chép được nhiều người chấp nhận nằm ngoài thế giới Arduino, nhưng bạn có thể tích hợp các thư viện của bên thứ ba cung cấp chức năng tương tự với các mức độ phức tạp và yêu cầu nguồn lực khác nhau.

## Xem thêm:
- [Tài liệu tham khảo `Serial` của Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Ghi chép vào thẻ SD với Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Bộ chia dữ liệu SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: Một ví dụ thực tế về ghi chép từ xa với Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)

---
title:                "Ghi log"
aliases:
- vi/arduino/logging.md
date:                  2024-01-28T22:03:32.107661-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Ghi chép với Arduino: Là gì & Tại sao?

"Ghi chép" là việc lưu lại các sự kiện, giao dịch, hoặc hoạt động xảy ra theo thời gian trong một hệ thống. Lập trình viên sử dụng nó để gỡ lỗi, giám sát sức khỏe hệ thống, thu thập thống kê, hoặc thậm chí kiểm toán việc sử dụng, làm cho nó trở thành một thực hành không thể thiếu để duy trì và hiểu rõ hành vi của mã của họ dưới các điều kiện khác nhau.

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

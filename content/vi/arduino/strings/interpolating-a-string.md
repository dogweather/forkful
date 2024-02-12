---
title:                "Nội suy chuỗi ký tự"
aliases: - /vi/arduino/interpolating-a-string.md
date:                  2024-01-28T22:02:20.293035-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Nội suy chuỗi là việc kết hợp biến với văn bản. Lập trình viên thực hiện điều này nhằm tạo các chuỗi một cách linh hoạt, làm cho đầu ra động và dễ đọc.

## Cách thực hiện:

Arduino không có tính năng nội suy chuỗi tích hợp sẵn, nhưng bạn có thể đạt được kết quả tương tự bằng cách sử dụng `sprintf()` hoặc bằng cách nối chuỗi và biến lại với nhau.

```Arduino
char buffer[50]; // Đảm bảo đủ lớn để chứa chuỗi cuối cùng
int sensorValue = analogRead(A0);
sprintf(buffer, "Đọc giá trị cảm biến: %d", sensorValue);
Serial.println(buffer);
```

Kết quả:
```
Đọc giá trị cảm biến: 402
```

Hoặc sử dụng nối chuỗi:

```Arduino
String message = "Đọc giá trị cảm biến: " + String(sensorValue);
Serial.println(message);
```

## Sâu hơn nữa

C và C++ (các ngôn ngữ cốt lõi của bản vẽ Arduino) truyền thống không có tính năng nội suy chuỗi như các ngôn ngữ mới hơn (ví dụ, Python hoặc JavaScript). Thay vào đó, `sprintf()` đã là cách thức chính để tạo chuỗi với biến. Nó hiệu quả, nhưng có thể hơi cồng kềnh và dễ mắc lỗi do tràn bộ đệm nếu không được quản lý cẩn thận.

Nối chuỗi bằng cách sử dụng lớp `String` intuitivel hơn và an toàn hơn khỏi lỗi bộ nhớ. Nhược điểm? Nó có thể dẫn đến phân mảnh bộ nhớ, đặc biệt là trong các chương trình chạy dài trên các thiết bị có bộ nhớ hạn chế như Arduino.

Một phương pháp thay thế tìm thấy trong một số thư viện C++ mới hơn hoặc chuyên biệt hơn (không phải tiêu chuẩn trong Arduino) là sử dụng các thư viện định dạng chuỗi cung cấp cú pháp gần gũi với nội suy, như `fmtlib`.

Về chi tiết thực hiện, khi bạn nối chuỗi bằng lớp `String`, ngầm định, Arduino đang tạo các đối tượng chuỗi mới và quản lý bộ nhớ cho bạn. `sprintf()`, mặt khác, viết văn bản đã định dạng vào một bộ đệm bạn cung cấp, cho bạn nhiều quyền kiểm soát hơn nhưng với giá phải tự quản lý bộ nhớ.

## Xem thêm

- Tham khảo lớp `String` của Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tham khảo hàm `sprintf()`: http://www.cplusplus.com/reference/cstdio/sprintf/
- Tối ưu hóa bộ nhớ Arduino: https://www.arduino.cc/en/Tutorial/Foundations/Memory
- fmtlib, một thư viện định dạng chuỗi hiện đại: https://fmt.dev/latest/index.html

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:08.948673-07:00
description: "Vi\u1EC7c \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Arduino\
  \ cho ph\xE9p b\u1EA1n truy c\u1EADp d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c l\u01B0\
  u tr\u1EEF tr\xEAn th\u1EBB SD ho\u1EB7c trong b\u1ED9 nh\u1EDB c\u1EE7a thi\u1EBF\
  t b\u1ECB\u2014ti\u1EC7n l\u1EE3i cho vi\u1EC7c c\xE0i \u0111\u1EB7t, d\u1EEF\u2026"
lastmod: '2024-03-11T00:14:10.309611-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Arduino\
  \ cho ph\xE9p b\u1EA1n truy c\u1EADp d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c l\u01B0\
  u tr\u1EEF tr\xEAn th\u1EBB SD ho\u1EB7c trong b\u1ED9 nh\u1EDB c\u1EE7a thi\u1EBF\
  t b\u1ECB\u2014ti\u1EC7n l\u1EE3i cho vi\u1EC7c c\xE0i \u0111\u1EB7t, d\u1EEF\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?

Việc đọc một tệp văn bản trong Arduino cho phép bạn truy cập dữ liệu được lưu trữ trên thẻ SD hoặc trong bộ nhớ của thiết bị—tiện lợi cho việc cài đặt, dữ liệu hiệu chuẩn, hoặc nhật ký. Lập trình viên làm điều này để tách biệt mã lệnh khỏi dữ liệu, làm cho việc cập nhật và quản lý trở nên dễ dàng hơn.

## Cách thực hiện:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // chờ cho đến khi cổng serial kết nối.
  }

  if (!SD.begin(4)) {
    Serial.println("Không khởi tạo được!");
    return;
  }

  myFile = SD.open("example.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Lỗi khi mở example.txt");
  }
}

void loop() {
  // không có gì xảy ra sau khi thiết lập
}
```

Kết quả mong đợi trên màn hình serial sẽ là nội dung của `example.txt` nếu mọi thứ được kết nối và khởi tạo đúng cách.

## Đi sâu hơn

Trong lịch sử, các bộ vi điều khiển như Arduino có bộ nhớ nhỏ và không thể xử lý tệp. Nhưng với các mô-đun thẻ SD và bộ nhớ trên bo mạch lớn hơn, chúng ta đã có chức năng vào/ra tệp. Có vài thư viện tồn tại cho mục đích này, chẳng hạn như `<SD.h>`. Nó được xây dựng dựa trên `<SPI.h>` để giao tiếp với thẻ SD qua bus SPI.

Về các phương án thay thế, bạn có thể sử dụng EEPROM (bộ nhớ không bay hơi) cho dữ liệu nhỏ hoặc thậm chí kết nối một Arduino với mạng và tải tệp từ một máy chủ. Thư viện `<SD.h>` là một bộ gói cho các chức năng cấp thấp hơn, xử lý quản lý tệp, đọc và viết một cách tương tự như luồng C++ chuẩn.

Triển khai trên Arduino bao gồm việc khởi tạo mô-đun thẻ SD, mở tệp, đọc nó cho đến khi không còn gì để đọc nữa, sau đó đóng nó để giải phóng tài nguyên. Việc xử lý lỗi là cần thiết, như việc không khởi tạo hoặc mở tệp không thành công, vì chúng là nguyên nhân phổ biến gây đau đầu trong các thao tác tệp.

## Xem thêm

- Tài liệu tham khảo thư viện SD chính thức: https://www.arduino.cc/en/Reference/SD
- Thư viện SPI của Arduino cho truyền thông nối tiếp: https://www.arduino.cc/en/reference/SPI
- Hướng dẫn sử dụng EEPROM với Arduino cho nhiệm vụ lưu trữ dữ liệu nhỏ: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:24.773603-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0\
  \ t\u1EA1o m\u1ED9t t\u1EC7p ch\u1EC9 c\u1EA7n trong m\u1ED9t kho\u1EA3ng th\u1EDD\
  i gian ng\u1EAFn ho\u1EB7c cho phi\xEAn l\xE0m vi\u1EC7c hi\u1EC7n t\u1EA1i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u\u2026"
lastmod: '2024-03-11T00:14:10.312288-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t t\u1EC7p ch\u1EC9 c\u1EA7n trong m\u1ED9t kho\u1EA3ng th\u1EDDi gian\
  \ ng\u1EAFn ho\u1EB7c cho phi\xEAn l\xE0m vi\u1EC7c hi\u1EC7n t\u1EA1i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Tạo một tệp tạm thời có nghĩa là tạo một tệp chỉ cần trong một khoảng thời gian ngắn hoặc cho phiên làm việc hiện tại. Lập trình viên làm điều này để lưu trữ dữ liệu trung gian mà không làm đầy bộ nhớ dài hạn hoặc cho dữ liệu chỉ cần tồn tại trong khi chương trình đang chạy.

## Làm thế nào:

Arduino thường tương tác với vi điều khiển không có hệ thống tập tin truyền thống—vì vậy "tệp" không được quản lý theo cùng một cách chúng ta làm trên PC. Thay vào đó, chúng ta sử dụng EEPROM (một lượng nhỏ bộ nhớ giữ được qua các lần khởi động lại) hoặc thẻ SD với một bảo vệ. Dưới đây là một ví dụ cơ bản về viết và đọc dữ liệu tạm thời vào EEPROM:

```Arduino
#include <EEPROM.h>

// Viết một giá trị tạm thời vào EEPROM
void writeTempEeprom(int address, byte value) {
  EEPROM.write(address, value);
}

// Đọc một giá trị tạm thời từ EEPROM
byte readTempEeprom(int address) {
  return EEPROM.read(address);
}

void setup() {
  // Khởi tạo truyền thông nối tiếp
  Serial.begin(9600);

  // Write and read from EEPROM
  writeTempEeprom(0, 123); // Ví dụ về giá trị và địa chỉ
  byte tempValue = readTempEeprom(0);

  // Xuất giá trị tạm thời
  Serial.print("Giá trị Tạm thời: ");
  Serial.println(tempValue);
}

void loop() {
  // Không có gì ở đây cho ví dụ này
}
```

Và nếu bạn làm việc với thẻ SD:

```Arduino
#include <SPI.h>
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // Đợi cho đến khi cổng nối tiếp kết nối. Chỉ cần thiết cho cổng USB gốc
  }

  if (!SD.begin(4)) {
    Serial.println("Khởi tạo thất bại!");
    return;
  }

  tempFile = SD.open("temp.txt", FILE_WRITE);

  // Viết điều gì đó vào tệp tạm thời
  if (tempFile) {
    tempFile.println("Chuỗi dữ liệu tạm thời");
    tempFile.close();
  } else {
    Serial.println("Lỗi mở temp.txt");
  }
  
  // Đọc từ tệp tạm thời
  tempFile = SD.open("temp.txt");
  if (tempFile) {
    while (tempFile.available()) {
      Serial.write(tempFile.read());
    }
    tempFile.close();
  } else {
    Serial.println("Lỗi mở temp.txt");
  }

  // Tùy chọn, loại bỏ tệp tạm thời sau khi sử dụng
  SD.remove("temp.txt");
}

void loop() {
  // Không có gì ở đây cho ví dụ này
}
```

Kết quả mẫu (cho cả hai ví dụ) trên Màn hình Nối tiếp sau khi chạy thiết lập sẽ là:
```
Giá trị Tạm thời: 123
```
Hoặc, cho ví dụ thẻ SD:
```
Chuỗi dữ liệu tạm thời
```

## Sâu hơn

Trong lịch sử, các tệp tạm thời trong lập trình đáp ứng nhu cầu như bộ nhớ đệm, nhật ký, hoặc giao tiếp giữa các quá trình. Trên các hệ thống như PC, với hệ điều hành đầy đủ, tệp tạm rất phổ biến. Trong Arduino, nó khác. Vi điều khiển có bộ nhớ không bay hơi hạn chế (EEPROM), hoặc chúng ta thêm bộ nhớ ngoại vi như thẻ SD.

Các phương án thay thế EEPROM cho dữ liệu ngắn hạn bao gồm sử dụng RAM (mất nhanh chóng giữa các chu kỳ nguồn và khởi động lại) hoặc bộ nhớ ngoại vi như Flash hoặc IC nối dây.

Về mặt thực hiện, khi ghi vào EEPROM trên Arduino, hãy nhớ rằng nó có chu kỳ ghi hạn chế (thường là khoảng 100,000 chu kỳ). Lạm dụng nó có thể làm hỏng nó—vì vậy hãy sử dụng nó một cách tiết kiệm cho các tình huống thực sự tạm thời.

Sử dụng thẻ SD cho bộ nhớ tạm thời giống như xử lý tệp thông thường trên PC. Nó cung cấp nhiều không gian hơn, nhưng yêu cầu quản lý đúng cách như đảm bảo chất lượng thẻ tốt, xử lý mở/đóng tệp một cách chính xác, và hiểu rằng nó tương đối chậm so với EEPROM hoặc RAM.

## Xem Thêm

- [Tham khảo Thư viện EEPROM](https://www.arduino.cc/en/Reference/EEPROM)
- [Tham khảo Thư viện SD](https://www.arduino.cc/en/Reference/SD)
- [Arduino File I/O](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- [Hiểu về bộ nhớ](https://learn.adafruit.com/memories-of-an-arduino)

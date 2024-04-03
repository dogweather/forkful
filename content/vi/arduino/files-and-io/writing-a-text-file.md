---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.634230-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Tr\u01B0\u1EDBc ti\xEAn, k\u1EBFt n\u1ED1\
  i \u0111\u1EA7u \u0111\u1ECDc th\u1EBB SD v\u1EDBi Arduino c\u1EE7a b\u1EA1n. Sau\
  \ \u0111\xF3 b\u1EA1n s\u1EBD c\u1EA7n th\u01B0 vi\u1EC7n SD. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t \u0111o\u1EA1n m\xE3 nhanh."
lastmod: '2024-03-13T22:44:37.013511-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc ti\xEAn, k\u1EBFt n\u1ED1i \u0111\u1EA7u \u0111\u1ECDc th\u1EBB\
  \ SD v\u1EDBi Arduino c\u1EE7a b\u1EA1n."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Cách thực hiện:
Trước tiên, kết nối đầu đọc thẻ SD với Arduino của bạn. Sau đó bạn sẽ cần thư viện SD. Dưới đây là một đoạn mã nhanh:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Bắt đầu giao tiếp nối tiếp
  Serial.begin(9600);
  
  // Kiểm tra khởi tạo thẻ SD
  if (!SD.begin(4)) {
    Serial.println("Khởi tạo thất bại!");
    return;
  }
  
  // Tạo/mở một tệp văn bản
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Nếu tệp được mở thành công, viết vào đó
  if (myFile) {
    myFile.println("Xin chào, thế giới!");
    myFile.close(); // Đóng tệp
    Serial.println("Viết xong.");
  } else {
    // Nếu tệp không mở được, in ra lỗi
    Serial.println("Lỗi mở test.txt");
  }
}

void loop() {
  // Không có gì ở đây
}
```

Kết quả mẫu sẽ là "Viết xong." trên màn hình nối tiếp, và "Xin chào, thế giới!" trong "test.txt" trên thẻ SD.

## Đào sâu
Theo lịch sử, ràng buộc về bộ nhớ của Arduino đã làm cho việc lưu trữ dữ liệu trở nên khó khăn. Với các mô-đun hiện đại và thẻ SD, việc này trở nên đơn giản hơn. Các phương án thay thế như EEPROM hoặc truyền trực tiếp đến máy tính cũng tốt nhưng có hạn (EEPROM có thể hỏng, truyền dữ liệu cần kết nối). Viết vào tệp là một cách thẳng thắn với `SD.h` nhưng hãy nhớ: thư viện sử dụng khá nhiều bộ nhớ, vì vậy nó phù hợp hơn cho các bảng có nhiều SRAM hơn.

## Xem thêm
Để biết thêm thông tin, hãy kiểm tra những điều sau:
- Tài liệu chính thức của thư viện SD: https://www.arduino.cc/en/Reference/SD
- Hướng dẫn kết nối mô-đun thẻ SD chi tiết: https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial
- Lớp File của Arduino cho các thao tác tệp: https://www.arduino.cc/en/Reference/File

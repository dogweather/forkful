---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.634230-07:00
description: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n tr\xEAn Arduino\
  \ c\xF3 ngh\u0129a l\xE0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1\
  ng v\u0103n b\u1EA3n trong m\u1ED9t t\u1EC7p, th\u01B0\u1EDDng l\xE0 tr\xEAn th\u1EBB\
  \ SD. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
lastmod: 2024-02-19 22:04:56.214170
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n tr\xEAn Arduino c\xF3\
  \ ngh\u0129a l\xE0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng\
  \ v\u0103n b\u1EA3n trong m\u1ED9t t\u1EC7p, th\u01B0\u1EDDng l\xE0 tr\xEAn th\u1EBB\
  \ SD. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tạo một tệp văn bản trên Arduino có nghĩa là lưu trữ dữ liệu dưới dạng văn bản trong một tệp, thường là trên thẻ SD. Các lập trình viên thực hiện điều này để lưu các dữ liệu như các bản đọc cảm biến để phân tích sau hoặc để ghi lại các sự kiện theo thời gian.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:04.439540-07:00
description: "L\xE0m Th\u1EBF N\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\u01B0\
  u d\u1EEF li\u1EC7u c\u1EA3m bi\u1EBFn v\xE0o m\u1ED9t t\u1EC7p CSV tr\xEAn th\u1EBB\
  \ SD."
lastmod: '2024-03-13T22:44:37.018579-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\u01B0u d\u1EEF li\u1EC7u c\u1EA3\
  m bi\u1EBFn v\xE0o m\u1ED9t t\u1EC7p CSV tr\xEAn th\u1EBB SD."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Làm Thế Nào:
Dưới đây là cách lưu dữ liệu cảm biến vào một tệp CSV trên thẻ SD:

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;
int sensorValue = analogRead(A0);  // giả sử giá trị cảm biến

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {  // Thẻ SD được kết nối với chân 4
    Serial.println("Thẻ SD lỗi hoặc không có mặt");
    return;
  }
  
  myFile = SD.open("data.csv", FILE_WRITE);
  
  if (myFile) {
    myFile.print("Thời Gian, Giá Trị Cảm Biến\n");
    unsigned long time = millis();
    myFile.print(time);
    myFile.print(", ");
    myFile.print(sensorValue);
    myFile.close();
    
    Serial.println("Dữ liệu đã được ghi vào thẻ SD.");
  } else {
    Serial.println("Lỗi khi mở tệp để viết.");
  }
}

void loop() {
  // Không có gì để làm ở đây
}
```

Dữ liệu CSV mẫu trong `data.csv`:
```
Thời Gian, Giá Trị Cảm Biến
12345, 678
```

## Sâu Hơn
Định dạng CSV có thể truy cứu về những ngày đầu của việc tính toán. Mặc dù có những lựa chọn tiên tiến hơn, như JSON hay XML, CSV vẫn là sự lựa chọn hàng đầu do tính đơn giản và hỗ trợ rộng rãi trên các nền tảng. Khi làm việc với Arduino, hãy nhớ về bộ nhớ hạn chế và chọn các thư viện CSV tối giản hoặc các hàm được viết riêng để phân tích và tạo dữ liệu CSV một cách hiệu quả.

## Xem Thêm
- Tài liệu tham khảo thư viện SD của Arduino: https://www.arduino.cc/en/reference/SD
- Phân tích CSV đơn giản trong C: https://github.com/robertgamble/simplecsv
- Hướng dẫn lưu dữ liệu Arduino vào Excel: https://www.instructables.com/Save-Arduino-sensor-data-to-a-text-file/

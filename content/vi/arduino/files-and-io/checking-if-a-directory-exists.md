---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:32.708602-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i hay kh\xF4ng l\xE0 vi\u1EC7c x\xE1c minh s\u1EF1 hi\u1EC7n di\u1EC7n\
  \ c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c tr\xEAn b\u1ED9 nh\u1EDB c\u1EE7a b\u1EA1\
  n tr\u01B0\u1EDBc khi b\u1EA1n th\u1EF1c hi\u1EC7n m\u1ED9t s\u1ED1 thao\u2026"
lastmod: 2024-02-19 22:04:56.208324
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i hay kh\xF4ng l\xE0 vi\u1EC7c x\xE1c minh s\u1EF1 hi\u1EC7n di\u1EC7n c\u1EE7a\
  \ m\u1ED9t th\u01B0 m\u1EE5c tr\xEAn b\u1ED9 nh\u1EDB c\u1EE7a b\u1EA1n tr\u01B0\
  \u1EDBc khi b\u1EA1n th\u1EF1c hi\u1EC7n m\u1ED9t s\u1ED1 thao\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Việc kiểm tra xem một thư mục có tồn tại hay không là việc xác minh sự hiện diện của một thư mục trên bộ nhớ của bạn trước khi bạn thực hiện một số thao tác với nó. Các lập trình viên làm điều này để tránh lỗi, như cố gắng tạo một thư mục đã tồn tại, hoặc truy cập một thư mục không tồn tại.

## Cách thực hiện:
Làm việc với thư mục trên Arduino thường liên quan đến thư viện SD để lưu trữ trên thẻ SD. Đầu tiên, hãy đảm bảo Arduino của bạn đã được kết nối đúng cách với mô-đun thẻ SD. Sau đó, bạn sử dụng hàm `SD.exists()` để kiểm tra sự tồn tại của một thư mục. Dưới đây là một ví dụ nhanh:
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // chờ đợi cổng serial kết nối. Chỉ cần thiết cho cổng USB native
  }

  if (!SD.begin(4)) { // Hãy chắc chắn sử dụng đúng chân chip select
    Serial.println("Khởi tạo thất bại!");
    return;
  }

  if (SD.exists("/example")) {
    Serial.println("Thư mục /example tồn tại.");
  } else {
    Serial.println("Thư mục /example không tồn tại.");
  }
}

void loop() {
  // Không cần làm gì tại đây
}
```
Đầu ra mẫu khi thư mục tồn tại:
```
Thư mục /example tồn tại.
```
Và khi nó không tồn tại:
```
Thư mục /example không tồn tại.
```
Hãy nhớ thay thế `/example` bằng đường dẫn thực tế bạn muốn kiểm tra.

## Sâu hơn nữa
Từ thời xa xưa, việc kiểm tra sự tồn tại của một thư mục không phải lúc nào cũng đơn giản. Các hệ thống có các lệnh đa dạng. Trong trường hợp của Arduino, thư viện SD làm cho nó nhất quán, kế thừa các khái niệm từ các thực hành lập trình chuẩn.

Về các phương án thay thế, nếu bạn làm việc với bộ nhớ không phải SD hoặc cần nhiều kiểm soát hơn, các thư viện khác như SdFat cung cấp chức năng tương tự với các tính năng bổ sung. Một số triển khai tiên tiến có thể tương tác trực tiếp với hệ thống tệp nhiều hơn, nhưng đối với hầu hết người dùng, SD.exists() là đủ.

Việc kiểm tra một thư mục liên quan đến việc thư viện yêu cầu hệ thống tệp tra cứu một mục tệp đặc biệt đại diện cho thư mục. Nếu nó có ở đó, tuyệt vời. Nếu không, bạn sẽ nhận được một giá trị false. Thư viện SD xử lý giao tiếp cấp thấp giữa Arduino của bạn và hệ thống tệp của phương tiện lưu trữ, che giấu các chi tiết phức tạp - để bạn có được thông tin bạn cần mà không phải lo lắng.

## Xem thêm
- Tham khảo Thư viện SD của Arduino: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- Thư viện SdFat cho tương tác thẻ SD mạnh mẽ hơn: [https://github.com/greiman/SdFat](https://github.com/greiman/SdFat)

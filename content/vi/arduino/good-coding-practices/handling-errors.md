---
title:                "Xử lý lỗi"
aliases:
- /vi/arduino/handling-errors.md
date:                  2024-01-28T22:01:58.907486-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Xử lý lỗi trong các chương trình của bạn bắt lấy những điều không lường trước sẽ cố gắng làm bạn vấp ngã. Bạn làm điều đó để giữ cho Arduino của bạn không bị hỏng khi điều không mong đợi xảy ra.

## Làm thế nào:

Giả sử Arduino của bạn đang đọc một cảm biến có thể thỉnh thoảng sản xuất ra các giá trị ngoài phạm vi. Dưới đây là cách bạn có thể xử lý điều đó:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Giá trị nằm trong phạm vi, tiếp tục với việc xử lý
  Serial.println(sensorValue);
} else {
  // Giá trị nằm ngoài phạm vi, xử lý lỗi
  Serial.println("Lỗi: Giá trị cảm biến nằm ngoài phạm vi.");
}
```
Mẫu Đầu ra:
```
523
Lỗi: Giá trị cảm biến nằm ngoài phạm vi.
761
```

## Đào sâu

Xử lý lỗi không luôn luôn đơn giản như vậy. Trong những ngày đầu, các nhà phát triển thường bỏ qua lỗi, dẫn đến "hành vi không xác định" đáng sợ. Khi lập trình tiến hóa, các công cụ cũng vậy — bạn bây giờ có ngoại lệ trong nhiều ngôn ngữ, nhưng chúng vẫn là cách 'kiểm tra trước' kiểu cũ trong thế giới Arduino do ràng buộc phần cứng và nguồn gốc C++.

Trong lập trình Arduino, bạn thường thấy câu lệnh `if-else` được dùng cho việc xử lý lỗi. Nhưng còn có các lựa chọn khác: sử dụng hàm `assert` để dừng thực thi nếu một điều kiện không đạt được hoặc thiết kế các hệ thống an toàn trong chính cấu hình phần cứng của bạn.

Khi thực hiện xử lý lỗi, hãy xem xét ảnh hưởng của việc dừng chương trình so với việc cho phép nó tiếp tục với một trạng thái mặc định hoặc an toàn. Có sự đánh đổi, và lựa chọn đúng phụ thuộc vào tác hại tiềm ẩn của việc gián đoạn so với hoạt động không chính xác.

## Xem thêm

Nâng cao kiến thức về phát hiện và xử lý lỗi với những nguồn sau:

- Tham khảo Ngôn ngữ Arduino: https://www.arduino.cc/reference/en/
- Cái nhìn sâu sắc hơn vào xử lý lỗi của Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Xử lý Lỗi C++: https://en.cppreference.com/w/cpp/error/exception

Điều này sẽ cung cấp cho bạn kiến thức và tự tin để tránh những cạm bẫy của lỗi trong những cuộc phiêu lưu Arduino của bạn.

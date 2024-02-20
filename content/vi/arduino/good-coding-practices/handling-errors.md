---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:58.907486-07:00
description: "X\u1EED l\xFD l\u1ED7i trong c\xE1c ch\u01B0\u01A1ng tr\xECnh c\u1EE7\
  a b\u1EA1n b\u1EAFt l\u1EA5y nh\u1EEFng \u0111i\u1EC1u kh\xF4ng l\u01B0\u1EDDng\
  \ tr\u01B0\u1EDBc s\u1EBD c\u1ED1 g\u1EAFng l\xE0m b\u1EA1n v\u1EA5p ng\xE3. B\u1EA1\
  n l\xE0m \u0111i\u1EC1u \u0111\xF3 \u0111\u1EC3 gi\u1EEF cho Arduino c\u1EE7a b\u1EA1\
  n kh\xF4ng\u2026"
lastmod: 2024-02-19 22:04:56.198429
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i trong c\xE1c ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1\
  n b\u1EAFt l\u1EA5y nh\u1EEFng \u0111i\u1EC1u kh\xF4ng l\u01B0\u1EDDng tr\u01B0\u1EDB\
  c s\u1EBD c\u1ED1 g\u1EAFng l\xE0m b\u1EA1n v\u1EA5p ng\xE3. B\u1EA1n l\xE0m \u0111\
  i\u1EC1u \u0111\xF3 \u0111\u1EC3 gi\u1EEF cho Arduino c\u1EE7a b\u1EA1n kh\xF4ng\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
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

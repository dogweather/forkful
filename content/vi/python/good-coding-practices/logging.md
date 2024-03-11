---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:23.375531-07:00
description: "Logging l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n\
  \ c\u1EE7a \u1EE9ng d\u1EE5ng trong khi m\u1ED9t ch\u01B0\u01A1ng tr\xECnh \u0111\
  ang ch\u1EA1y, cung c\u1EA5p m\u1ED9t d\u1EA5u v\u1EBFt \u0111\u1EC3 ph\xE2n t\xED\
  ch sau s\u1EF1 c\u1ED1 v\xE0 gi\xE1m s\xE1t th\u1EDDi\u2026"
lastmod: '2024-03-11T00:14:09.336890-06:00'
model: gpt-4-0125-preview
summary: "Logging l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n c\u1EE7\
  a \u1EE9ng d\u1EE5ng trong khi m\u1ED9t ch\u01B0\u01A1ng tr\xECnh \u0111ang ch\u1EA1\
  y, cung c\u1EA5p m\u1ED9t d\u1EA5u v\u1EBFt \u0111\u1EC3 ph\xE2n t\xEDch sau s\u1EF1\
  \ c\u1ED1 v\xE0 gi\xE1m s\xE1t th\u1EDDi\u2026"
title: Ghi log
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Logging là quá trình ghi lại các sự kiện của ứng dụng trong khi một chương trình đang chạy, cung cấp một dấu vết để phân tích sau sự cố và giám sát thời gian thực. Lập trình viên thực hiện việc này bởi vì nó giúp gỡ lỗi, giám sát hiệu suất và theo dõi hành động của người dùng cho mục đích an ninh và phân tích.

## Làm thế nào:
Python đi kèm với một mô-đun tích hợp sẵn cho việc logging. Dưới đây là cách thiết lập cơ bản:
```Python
import logging

# Cấu hình cơ bản cho logging
logging.basicConfig(level=logging.INFO)

# Ghi lại các tin nhắn
logging.debug('Đây là một thông điệp gỡ lỗi')
logging.info('Thông tin về những gì chương trình của bạn vừa làm')
logging.warning('Một thông điệp cảnh báo')
logging.error('Đã xảy ra lỗi')
logging.critical('Chương trình không thể phục hồi!')
```
Khi bạn chạy code này, bạn sẽ thấy đầu ra sau (do mức độ mặc định là WARNING, thông điệp debug và info sẽ không được hiển thị):
```
WARNING:root:Một thông điệp cảnh báo
ERROR:root:Đã xảy ra lỗi
CRITICAL:root:Chương trình không thể phục hồi!
```
Bạn cũng có thể thiết lập logging để ghi vào một tệp thay vì bảng điều khiển:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Giờ đây, nhật ký của bạn sẽ được hướng dẫn đến tệp 'app.log'.

## Đi sâu vào vấn đề
Logging đã tồn tại từ những ngày đầu của lập trình, với hệ thống nhật ký là một trong những hình thức lưu trữ lâu đời nhất bên ngoài các tệp thực sự chứa dữ liệu. Bỏ qua lịch sử, khái niệm chính của việc logging về cơ bản không thay đổi, mặc dù các công cụ đã phát triển.

Mô-đun `logging` của Python khá mạnh mẽ và linh hoạt. Nó cho phép lập trình viên đặt các mức độ log khác nhau (DEBUG, INFO, WARNING, ERROR, CRITICAL) giúp phân loại và lọc nhật ký. Nó có một hệ thống logger phân cấp, nghĩa là bạn có thể có mối quan hệ cha-con giữa các logger và truyền tin nhắn lên chuỗi.

Các lựa chọn thay thế bao gồm các thư viện của bên thứ ba như Loguru hoặc structlog, cung cấp các tính năng nâng cao và giao diện đơn giản hơn mô-đun logging tích hợp sẵn. Chúng có thể cung cấp đầu ra đẹp hơn, việc chuẩn hóa dữ liệu có cấu trúc tốt hơn và các cách tiếp cận trực quan hơn để thực hiện cấu hình log.

Về việc triển khai, khi thiết lập logging, điều quan trọng là phải làm điều này một lần ở đầu ứng dụng của bạn. Cấu hình nó ở mức độ module được khuyến nghị sử dụng `logging.getLogger(__name__)` để tuân theo các phong cách tốt nhất của Python logging.

Logging không nên ảnh hưởng đến hiệu suất của một ứng dụng dưới điều kiện bình thường. Tuy nhiên, cần phải cẩn thận với những gì được ghi: việc logging quá mức, đặc biệt là ở mức độ DEBUG, có thể làm chậm ứng dụng và nhanh chóng lấp đầy không gian lưu trữ tệp nhật ký.

## Tham khảo thêm
Để biết thêm về mô-đun logging của Python, hãy kiểm tra cuốn sách dạy nấu ăn logging chính thức của Python để xem một số ví dụ tuyệt vời và các phong cách tốt nhất: https://docs.python.org/3/howto/logging-cookbook.html

Để có cái nhìn sâu sắc về logging có cấu trúc và cách nó có thể giúp nhật ký thông tin hơn và dễ phân tích hơn, Loguru được tài liệu hóa tốt: https://loguru.readthedocs.io

Ngoài ra, cũng cân nhắc đến phương pháp luận ứng dụng 12 yếu tố, cụ thể là mục về nhật ký cho cách nhìn hiện đại về logging ứng dụng: https://12factor.net/logs

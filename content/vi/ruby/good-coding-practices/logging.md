---
title:                "Ghi log"
aliases: - /vi/ruby/logging.md
date:                  2024-01-28T22:03:21.294276-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Ghi chép và Lý do
Ghi chép trong lập trình như là việc giữ một nhật ký cho ứng dụng của bạn. Đó là sự ghi chép có hệ thống các sự kiện, thông điệp và điểm dữ liệu mà cho bạn cái nhìn sâu sắc về những gì ứng dụng của bạn đang làm và cách nó hoạt động. Các lập trình viên ghi chép vì nó rất quan trọng để debug, giám sát sức khỏe ứng dụng, và nhận biết về những vấn đề tiềm ẩn trước khi chúng trở thành những vấn đề thực sự.

## Cách thức:
Ruby đi kèm với một mô-đun được tích hợp sẵn cho việc ghi chép, `Logger`, rất dễ sử dụng. Dưới đây là một ví dụ nhanh để bạn bắt đầu:

```ruby
require 'logger'

# Tạo một Logger ghi ra STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Ví dụ về các thông điệp log
logger.info("Đây là thông điệp thông tin")
logger.warn("Đây là thông điệp cảnh báo")
logger.error("Đây là thông điệp lỗi")
```

Việc chạy script trên sẽ xuất ra điều gì đó giống như sau:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Đây là thông điệp thông tin
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Đây là thông điệp cảnh báo
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Đây là thông điệp lỗi
```

Bạn có thể cấu hình định dạng và cấp độ log để lọc bỏ những thông tin không cần thiết, và bạn có thể hướng các log tới các đầu ra khác nhau, như một tệp hoặc thậm chí một dịch vụ ghi chép bên ngoài.

## Tìm hiểu sâu hơn
Ghi chép là một truyền thống cổ xưa trong lập trình. Trong lịch sử, các log là những tệp văn bản đơn giản, được phân tích một cách thủ công với các công cụ như `grep`. Nhưng khái niệm này đã phát triển thành một hệ sinh thái toàn bộ các framework và dịch vụ ghi chép mạnh mẽ như Log4j, Syslog trên Linux, hoặc Sematext và Loggly trong kỷ nguyên đám mây.

`Logger` của Ruby là một cách không rườm rà để bắt đầu, nhưng nếu bạn cần nhiều sức mạnh và linh hoạt hơn, bạn có thể xem xét các lựa chọn khác như Lograge hay Semantic Logger. Những thư viện này hoạt động tốt với các ứng dụng Ruby, cung cấp kiểm soát tinh vi hơn đối với việc định dạng log, bao gồm cả log có cấu trúc (định dạng JSON), hiệu suất tốt hơn và tích hợp mượt mà với các dịch vụ khác.

Mỗi thư viện ghi chép Ruby có cách tiếp cận riêng của mình, nhưng về cơ bản, chúng đều xoay quanh ý tưởng về một thể hiện logger mà bạn gửi thông điệp tới. Logger xử lý những thông điệp này dựa trên các cấp độ được thiết lập—DEBUG, INFO, WARN, ERROR, FATAL, và UNKNOWN—và quyết định làm gì với chúng: in ra, lưu vào tệp, gửi qua mạng, v.v.

## Xem thêm
Để tìm hiểu sâu về mô-đun ghi chép được tích hợp sẵn của Ruby, hãy xem tài liệu chính thức:

Nếu bạn quan tâm đến việc ghi chép nâng cao hơn hoặc muốn khám phá các gem bên thứ ba:
- [Lograge](https://github.com/roidrage/lograge)

Đối với các thực hành và triết lý ghi chép chung (không riêng gì Ruby), những bài viết dưới đây là những tài liệu đáng đọc:
- [Cuốn sách về Kỹ sư Độ tin cậy Website của Google - Chương 16: Xử lý Quá tải](https://sre.google/sre-book/handling-overload/#log-messages)
- [Ứng dụng 12 Yếu tố - Nhật ký](https://12factor.net/logs)

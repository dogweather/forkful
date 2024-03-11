---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:52.756221-07:00
description: "Logging l\xE0 vi\u1EC7c ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n, l\u1ED7\
  i v\xE0 th\xF4ng tin \u0111\xE1ng k\u1EC3 kh\xE1c t\u1EEB qu\xE1 tr\xECnh ch\u1EA1\
  y c\u1EE7a m\u1ED9t ch\u01B0\u01A1ng tr\xECnh v\xE0o m\u1ED9t t\u1EC7p ho\u1EB7\
  c m\u1ED9t lu\u1ED3ng \u0111\u1EA7u ra. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-11T00:14:10.178159-06:00'
model: gpt-4-0125-preview
summary: "Logging l\xE0 vi\u1EC7c ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n, l\u1ED7i\
  \ v\xE0 th\xF4ng tin \u0111\xE1ng k\u1EC3 kh\xE1c t\u1EEB qu\xE1 tr\xECnh ch\u1EA1\
  y c\u1EE7a m\u1ED9t ch\u01B0\u01A1ng tr\xECnh v\xE0o m\u1ED9t t\u1EC7p ho\u1EB7\
  c m\u1ED9t lu\u1ED3ng \u0111\u1EA7u ra. C\xE1c l\u1EADp tr\xECnh\u2026"
title: Ghi log
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Logging là việc ghi lại các sự kiện, lỗi và thông tin đáng kể khác từ quá trình chạy của một chương trình vào một tệp hoặc một luồng đầu ra. Các lập trình viên thực hiện việc này để theo dõi hành vi của các ứng dụng của họ, gỡ lỗi sự cố, và bảo trì một bản ghi lịch sử của các hoạt động có thể hỗ trợ cho việc khắc phục sự cố trong tương lai.

## Cách thực hiện:

Trong Bash, việc logging có thể đơn giản là chuyển hướng hoặc thêm vào đầu ra vào một tệp. Dưới đây là một ví dụ cơ bản:

```Bash
echo "Bắt đầu script..." >> script.log
# Các lệnh script của bạn ở đây
echo "Script hoàn thành vào $(date)" >> script.log
```

Đối với cái gì đó nâng cao hơn, bạn có thể tích hợp syslog cho việc logging trên toàn hệ thống:

```Bash
logger "Tin nhắn tùy chỉnh từ script của tôi"
```

`logger` gửi một thông điệp log đến dịch vụ syslog, sau đó xử lý nó theo cấu hình syslog của hệ thống.

Mẫu đầu ra được ghi trong `script.log`:

```Bash
Bắt đầu script...
Script hoàn thành vào Tue Mar 23 09:26:35 PDT 2021
```

## Sâu hơn

Trong lịch sử của các hệ thống giống Unix, việc logging đã được hỗ trợ bởi dịch vụ syslog, cho phép các ứng dụng và các phần khác của hệ thống gửi các thông điệp log một cách trung tâm. Điều này cho phép sự triển khai một cơ chế logging tiêu chuẩn trên toàn hệ thống.

Khi nói đến các lựa chọn thay thế, một số người có thể xem xét sử dụng `syslog-ng` hoặc `rsyslog` để có các tính năng logging nâng cao hơn, hoặc ghi log vào một cơ sở dữ liệu chuỗi thời gian cho mục đích phân tích. Đối với các ứng dụng có mức độ phức tạp cao hơn, việc sử dụng một thư viện hoặc ứng dụng logging chuyên biệt như Log4j (trong hệ sinh thái Java) hoặc Monolog (trong PHP), có thể cung cấp các tùy chọn logging có cấu trúc và có thể cấu hình, thậm chí có lý hợp cho ngôn ngữ scripting như Bash.

Cách bạn thực hiện logging phụ thuộc rất nhiều vào yêu cầu của ứng dụng của bạn. Nếu bạn chỉ cần đầu ra đơn giản để theo dõi tiến độ của script, việc thêm thông điệp vào một tệp là dễ dàng và thuận tiện. Tuy nhiên, đối với việc logging dễ mở rộng và mạnh mẽ hơn, bạn sẽ muốn tích hợp với một hệ thống logging hỗ trợ các tính năng như xoay log, cấp độ log, và logging từ xa.

## Xem thêm

- Các trang `man` cho các hàm `logger` và `syslog` luôn là bạn tốt của bạn, hãy thử `man logger` hoặc `man syslog`.
- Để có cái nhìn sâu sắc về việc logging hệ thống, xem xét việc đọc tài liệu `rsyslog` và `syslog-ng`.
- Để tìm hiểu thêm về bối cảnh lịch sử và nguyên tắc đằng sau việc logging trong các hệ thống giống Unix, giao thức `Syslog` được tài liệu hóa trong RFC 5424 cung cấp thông tin toàn diện.

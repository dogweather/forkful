---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:12.702566-07:00
description: "Ghi log c\u01A1 b\u1EA3n l\xE0 ghi l\u1EA1i nh\u1EEFng g\xEC \u1EE9\
  ng d\u1EE5ng c\u1EE7a b\u1EA1n \u0111ang l\xE0m \u2013 c\xF3 th\u1EC3 coi \u0111\
  \xF3 nh\u01B0 nh\u1EADt k\xFD, nh\u01B0ng d\xE0nh cho m\xE3. L\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo d\xF5i nh\u1EEFng\u2026"
lastmod: 2024-02-19 22:04:56.453970
model: gpt-4-0125-preview
summary: "Ghi log c\u01A1 b\u1EA3n l\xE0 ghi l\u1EA1i nh\u1EEFng g\xEC \u1EE9ng d\u1EE5\
  ng c\u1EE7a b\u1EA1n \u0111ang l\xE0m \u2013 c\xF3 th\u1EC3 coi \u0111\xF3 nh\u01B0\
  \ nh\u1EADt k\xFD, nh\u01B0ng d\xE0nh cho m\xE3. L\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo d\xF5i nh\u1EEFng\u2026"
title: Ghi log
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Ghi log cơ bản là ghi lại những gì ứng dụng của bạn đang làm – có thể coi đó như nhật ký, nhưng dành cho mã. Lập trình viên làm điều này để theo dõi những chi tiết nhỏ nhặt, như thay đổi trạng thái, sự kiện hệ thống và lỗi khó chịu, đảm bảo không có sự cố nào lọt qua một cách không được chú ý.

## Làm thế nào:
Trong Fish, việc ghi log có thể đơn giản như là chuyển hướng dòng xuất chuẩn và lỗi đến một tệp. Hãy tạo một bản ghi cho thời gian bắt đầu và kết thúc của script của chúng ta.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script started" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script ended" >> my_app.log
end

log_start
# ... nhiệm vụ của script của bạn ...
log_end

cat my_app.log
```

Đây là những gì bạn sẽ thấy trong `my_app.log`:

```
2023-04-01 10:35:47  - Script started
2023-04-01 10:36:02  - Script ended
```

Đối với việc ghi log nâng cao, bạn có thể sử dụng các hàm với tham số cho mức độ log và thông điệp:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Đây là một thông điệp thông tin."
log_message ERROR "Đã xảy ra lỗi!"
```

Mẫu đầu ra `my_app.log` sẽ là:
```
2023-04-01 10:35:47 [INFO] Đây là một thông điệp thông tin.
2023-04-01 10:35:49 [ERROR] Đã xảy ra lỗi!
```

## Tìm hiểu sâu hơn
Trong lịch sử, việc ghi log trong script shell được thực hiện với một loạt các câu lệnh `echo`, và mặc dù điều này chắc chắn vẫn là một lựa chọn, nhưng việc triển khai các hệ thống phức tạp hơn có thể là một thách thức. Fish không có cơ chế ghi log tích hợp như một số shells hoặc ngôn ngữ lập trình khác, vì vậy bạn thường cần tự tạo ra nó.

Các phương án thay thế cho lệnh `echo` tích hợp của Fish cho việc ghi log bao gồm các công cụ Unix như `syslog` hoặc `logger`, mà giao tiếp với daemon log hệ thống, cung cấp một cách tiếp cận tích hợp hơn để ghi log các sự kiện toàn hệ thống.

Sự đơn giản của Fish cho phép bạn tạo ra các hàm để xử lý độ chi tiết của việc ghi log, thiết lập các mức độ khác nhau mà bạn có thể bật hoặc tắt. Một số triển khai thậm chí có thể bao gồm tên của script, số dòng và dấu thời gian, giúp dễ dàng truy vết lại các bước dẫn đến một sự kiện.

## Xem thêm
- Tài liệu của Fish Shell về cách viết hàm: https://fishshell.com/docs/current/#syntax-function
- Mẹo Lập Script Shell Cơ bản: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Hướng dẫn về Giao thức Syslog: https://tools.ietf.org/html/rfc5424

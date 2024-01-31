---
title:                "Ghi log"
date:                  2024-01-28T22:03:12.702566-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

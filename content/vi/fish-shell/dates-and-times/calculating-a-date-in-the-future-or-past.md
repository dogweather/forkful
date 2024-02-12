---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
aliases:
- /vi/fish-shell/calculating-a-date-in-the-future-or-past/
date:                  2024-01-28T21:55:49.202160-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc thao tác với các ngày để tìm ra ngày nào đó đã qua hoặc sẽ đến. Các lập trình viên làm điều này cho việc lập lịch, nhắc nhở, hoặc xác định thời hạn và kỳ hạn.

## Cách thực hiện:
Đây là một cách hay để làm việc với ngày tháng trong Fish Shell:

```Fish Shell
# Thêm ngày vào ngày hiện tại
set -l days_to_add 10
date -d "+$days_to_add days"

# Ví dụ về đầu ra (thay đổi theo ngày hiện tại):
# Wed Mar 29 00:29:10 PDT 2023

# Trừ ngày từ ngày hiện tại
set -l days_to_subtract 10
date -d "-$days_to_subtract days"

# Ví dụ về đầu ra (lần nữa, ngày của bạn có thể thay đổi):
# Sun Mar 9 00:30:42 PDT 2023
```

## Tìm hiểu Sâu
Fish không chỉ dừng lại ở việc tạo ra tiếng vỗ; nó còn có lịch sử. Các shell như bash từng là lựa chọn hàng đầu cho việc tính toán ngày, thông thường qua GNU `date`. Fish, giữ nó một cách gọn gàng, sử dụng cú pháp tương tự nhưng có thể thân thiện với người dùng và dễ đọc hơn – tuyệt vời cho cả những người mới bắt đầu và những người có kinh nghiệm.

Các phương pháp thay thế cho việc tính toán ngày bao gồm việc sử dụng các ngôn ngữ lập trình như Python hoặc sử dụng `dateutils`. Mỗi phương pháp đều có những điểm mạnh riêng, mặc dù `dateutils` có thể hơi kém phổ biến và Python có thể quá mức cần thiết cho các tác vụ đơn giản. Việc triển khai trong Fish là ở giữa, với lệnh `date` mượn từ các tiêu chuẩn UNIX – nó gần như được cài đặt ở khắp mọi nơi và kết nối mượt mà với các cài đặt thời gian của hệ thống.

## Xem Thêm
Để biết thêm chi tiết, tham gia vào những cuộc khám phá này:
- [GNU Coreutils – Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): Hiểu rõ cách `date` hoạt động từ bên trong.
- [Tài Liệu của Fish Shell](https://fishshell.com/docs/current/index.html): Tài liệu chính thức, nơi bạn có thể tìm hiểu về Fish và các lệnh khác của nó.
- [StackOverflow: Toán học ngày tháng](https://stackoverflow.com/questions/tagged/date-arithmetic): Xem các vấn đề và giải pháp từ cộng đồng thực tế.

---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
aliases: - /vi/bash/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:33.339814-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ là để tìm ra ngày trước hoặc sau một khoảng thời gian nhất định. Lập trình viên thực hiện điều này cho các nhiệm vụ như thiết lập nhắc nhở, chạy các công việc theo lịch trình, hoặc xử lý ngày hết hạn.

## Làm thế nào:
Trong Bash, bạn có thể sử dụng lệnh `date` kèm theo cờ `-d` để thao tác với các ngày. Dưới đây là cách làm:

```Bash
# Ngày Hiện Tại
date

# Ngày Tương Lai: 10 ngày kể từ bây giờ
date -d "+10 days"

# Ngày Quá Khứ: 10 ngày trước
date -d "-10 days"

# Ngày Tương Lai Cụ Thể: Thêm tuần, tháng, năm
date -d "+1 month"
date -d "+2 weeks"
date -d "+1 year"

# Đầu ra mẫu cho ngày tương lai
Mon 31 Jan 2023 12:34:56 PM PST
```

## Sâu hơn
Việc thao tác với các ngày là một yêu cầu phổ biến trong lập trình và viết kịch bản. Trong lịch sử, nhiệm vụ này trở nên cồng kềnh và dễ mắc lỗi hơn khi xử lý năm nhuận, múi giờ, v.v. Trong các hệ thống giống Unix, lệnh `date` đã phát triển để bao gồm các tùy chọn dễ dàng cho việc tính toán ngày.

Các phương pháp thay thế bao gồm sử dụng toán học shell hoặc các công cụ bên ngoài như `awk` hoặc `perl` cho lô-gíc ngày phức tạp hơn, nhưng lệnh `date` vẫn là cách dễ dàng và trực tiếp nhất cho các hoạt động cơ bản. Đằng sau hậu trường, lệnh `date` sử dụng các thư viện hệ thống để xử lý sự phức tạp của việc tính toán thời gian, trừu tượng hóa điều này khỏi người dùng.

## Xem Thêm
- Hướng dẫn Coreutils GNU về Ngày: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Nhiều ví dụ và trường hợp sử dụng: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Hướng dẫn Lập trình Bash Nâng Cao: https://tldp.org/LDP/abs/html/abs-guide.html

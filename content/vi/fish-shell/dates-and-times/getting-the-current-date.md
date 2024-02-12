---
title:                "Lấy ngày hiện tại"
aliases:
- vi/fish-shell/getting-the-current-date.md
date:                  2024-01-28T22:01:24.785388-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Lấy ngày hiện tại có nghĩa là giành lấy ngày lịch sự hiện tại từ hệ thống của bạn. Các lập trình viên làm điều này để đánh dấu thời gian các sự kiện, lên lịch các công việc, hoặc chỉ đơn giản là hiển thị ngày cho người dùng.

## Làm thế nào:

Trong Fish Shell, việc bắt ngày hiện tại rất dễ dàng. Sử dụng lệnh `date`:

```fish
# Lấy ngày hiện tại ở định dạng mặc định
date

# Kết quả mẫu
Wed Apr 5 15:26:42 PDT 2023

# Lấy ngày hiện tại trong định dạng tùy chỉnh, ví dụ, YYYY-MM-DD
date "+%Y-%m-%d"

# Kết quả mẫu
2023-04-05
```

Nếu bạn muốn gán nó vào một biến, chỉ cần làm:

```fish
# Lưu trữ ngày hiện tại vào một biến
set current_date (date "+%Y-%m-%d")

# In biến
echo $current_date

# Kết quả mẫu
2023-04-05
```

## Sâu hơn nữa

Lịch sử, lệnh `date` đến từ UNIX, và nó đã tồn tại hàng thập kỷ. Trong Fish Shell, bạn đang sử dụng phiên bản thân thiện hơn của công cụ cổ xưa này. Định dạng `%Y-%m-%d` cho lệnh `date` cung cấp cho bạn năm, tháng và ngày, nhưng bạn có rất nhiều tùy chọn khác như `%H` cho giờ hoặc `%M` cho phút.

Tại sao sử dụng Fish thay vì Bash hoặc Zsh cho việc này? Nà, Fish được biết đến với cú pháp đơn giản hơn, dễ đọc hơn. Ví dụ, việc thiết lập các biến rõ ràng hơn nhiều (`set varname value` so với `varname=value`), và bạn không cần phải tiền tố bằng `$` khi sử dụng chúng.

Các phương án thay thế cho `date` tích hợp trong Fish có thể bao gồm việc cài đặt các công cụ nặng hơn như `GNU date` để có thêm tính năng hoặc sử dụng các chức năng khác của Fish hoặc thậm chí là các chương trình bên ngoài nếu bạn cần hành vi tùy chỉnh hơn.

Về mặt thực hiện, khi bạn chạy `date` trong Fish, bạn đang sử dụng bao bọc Fish quanh lệnh date của hệ thống. Điều đó có nghĩa là trên Linux, bạn có khả năng đang sử dụng `GNU date`, trong khi trên macOS, bạn đang sử dụng phiên bản BSD. Chúng khá giống nhau, nhưng có một số khác biệt nhỏ trong các tùy chọn hỗ trợ.

## Xem thêm

- [Tài liệu Fish Shell](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)

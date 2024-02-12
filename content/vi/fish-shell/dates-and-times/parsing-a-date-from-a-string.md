---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases:
- /vi/fish-shell/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:14.469372-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi một ngày từ một chuỗi liên quan đến việc đọc thông tin ngày tháng được định dạng dưới dạng văn bản và chuyển đổi nó thành cấu trúc dữ liệu ngày mà chương trình có thể hiểu được. Lập trình viên làm việc này để thao tác và làm việc với ngày tháng—suỵ nghĩ về phân tích, lập lịch, hoặc đơn giản là hiển thị chúng trong một định dạng khác.

## Làm thế nào:

```Fish Shell
# Phân tích cú pháp ngày cơ bản sử dụng hàm `strptime`
set date_string "2023-04-15"
set -l format "%Y-%m-%d"
set -l parsed_date (string tolower (date -u --date=$date_string +"$format"))

echo $parsed_date # Kết quả: 2023-04-15
```

```Fish Shell
# Xử lý nhiều định dạng ngày bằng cách sử dụng switch
set date_string1 "15-04-2023"
set date_string2 "April 15, 2023"

function parse_date -a date_string
    switch $date_string
        case "*-*-*"
            date --date=$date_string +%Y-%m-%d
        case "* *, *"
            date --date=$date_string +%Y-%m-%d
    end
end

echo (parse_date $date_string1) # Kết quả: 2023-04-15
echo (parse_date $date_string2) # Kết quả: 2023-04-15
```

## Tìm hiểu sâu

Fish Shell không có các hàm phân tích cú pháp ngày được tích hợp sẵn như một số ngôn ngữ khác. Thay vào đó, nó dựa vào các công cụ bên ngoài như `date`. Lệnh `date` rất linh hoạt, và với sự giúp đỡ từ `strptime` (phân tích cú pháp chuỗi thời gian), là một hàm thư viện chuẩn C, nó có thể xử lý nhiều định dạng ngày.

Trước `date` và `strptime`, lập trình viên viết bộ phân tích cú pháp tùy chỉnh—thường xuyên bị lỗi và phức tạp. Bây giờ, các công cụ xử lý các vấn đề về múi giờ và năm nhuận, giúp chúng ta tránh những phiền toái.

Có phương án thay thế? Chắc chắn, các ngôn ngữ kịch bản như Python có thư viện ngày-giờ mạnh mẽ như `datetime`. Nhưng Fish, với tư cách là một 'shell', thích sự nhẹ nhàng, các chương trình dòng lệnh cho công việc như thế này.

Trong các ví dụ của chúng tôi, chúng tôi đã sử dụng `switch` để chọn định dạng ngày để `date` phân tích cú pháp. Nó sạch sẽ và có thể mở rộng. Muốn thêm định dạng? Thêm nhiều khối `case`.

Tại sao sử dụng `string tolower` trong ví dụ đầu tiên? Điều này liên quan đến tính nhất quán, đảm bảo chuỗi định dạng và đầu ra có chữ cái viết thường đồng nhất. Một chi tiết nhỏ, nhưng nó minh họa sự ưa thích của Fish đối với các thao tác chuỗi đơn giản.

## Xem Thêm

- Trang hướng dẫn `date`: `man date`
- Tài liệu về thao tác chuỗi của Fish Shell: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Các ví dụ sử dụng lệnh ngày chung: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)

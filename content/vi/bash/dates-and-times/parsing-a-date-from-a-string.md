---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases: - /vi/bash/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:04.214419-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Phân tích cú pháp ngày từ một chuỗi nghĩa là trích xuất các thành phần ngày—như ngày, tháng và năm—từ văn bản. Các lập trình viên thực hiện điều này để thao tác hoặc hiển thị các ngày theo các định dạng khác nhau hoặc để tính toán sự khác biệt về thời gian trong các kịch bản.

## Làm thế nào:

Sử dụng `date` với `+%Y-%m-%d` cho chúng ta một định dạng đầu ra:

```Bash
date_str="Jan 01 2023"
formatted_date=$(date -d "$date_str" '+%Y-%m-%d')
echo $formatted_date
```
```
2023-01-01
```

`date -d` cho phép chúng ta phân tích cú pháp chuỗi của mình, trong khi `+%Y-%m-%d` chỉ định định dạng đầu ra.

## Sâu hơn

Bash bản thân không giỏi lắm trong việc phân tích cú pháp ngày. Lịch sử, các hệ thống Unix không bao gồm một công cụ tích hợp cho điều này. Hầu hết các kịch bản phụ thuộc vào các công cụ bên ngoài hoặc các giải pháp phức tạp. GNU `date` đã thay đổi cuộc chơi với tùy chọn `-d` của mình, cho phép dễ dàng phân tích cú pháp ngày và định dạng đầu ra.

Có các lựa chọn khác? Chắc chắn, có `awk`, `sed`, và `perl`. Mỗi cái đều có cách riêng của mình để giải quyết vấn đề, nhưng `date` thường là lựa chọn đầu tiên do sự đơn giản.

Chi tiết triển khai còn thú vị hơn. `date` mặc định sử dụng các thiết lập vùng địa lý của hệ thống, ảnh hưởng đến cách nó giải thích đầu vào. Việc ghi đè vùng địa lý có thể cần thiết để có hành vi nhất quán trên các môi trường khác nhau. Thêm vào đó, xử lý các ngày trước năm 1970 hoặc sau năm 2038? Đó là nơi mà mọi thứ có thể trở nên lỗi lầm do các hạn chế về dấu thời gian Unix.

## Xem thêm

- Trang man của GNU `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Thêm về dấu thời gian Unix và vấn đề Y2038: https://en.wikipedia.org/wiki/Year_2038_problem
- Phân tích cú pháp ngày trong `awk`: https://www.gnu.org/software/gawk/manual/html_node/Time-Functions.html

---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
aliases:
- vi/bash/removing-quotes-from-a-string.md
date:                  2024-01-28T22:06:55.220117-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Loại bỏ dấu ngoặc khỏi một chuỗi bao gồm việc gỡ bỏ các dấu ngoặc kép bao quanh chuỗi đó. Các lập trình viên thường muốn làm điều này để làm sạch dữ liệu đầu vào, chuẩn bị dữ liệu cho mục đích so sánh, hoặc tuân thủ một định dạng dữ liệu cụ thể khi giao tiếp với các chương trình hoặc hệ thống khác.

## Cách thực hiện:
Bash có một số cách để loại bỏ dấu ngoặc khỏi chuỗi. Dưới đây là một số ví dụ nhanh:

```Bash
#!/bin/bash

# Sử dụng biến đổi biến để loại bỏ cả dấu ngoặc đơn và dấu ngoặc kép
STRING="\"Xin chào, Thế giới!\""
echo ${STRING//\"}

# Sử dụng `tr` để xóa dấu ngoặc
STRING="'Xin chào, Thế giới!'"
echo $STRING | tr -d "\'"

# Sử dụng `sed` để xóa dấu ngoặc
STRING="\"Xin chào, Thế giới!\""
echo $STRING | sed 's/"//g'
```

Kết quả mẫu:

```
Xin chào, Thế giới!
Xin chào, Thế giới!
Xin chào, Thế giới!
```

## Sâu hơn nữa
Ngày xưa, các lệnh Unix như `tr` và `sed` là các công cụ chính để xử lý văn bản. Chúng vẫn được sử dụng ngày nay vì tính linh hoạt và mạnh mẽ của mình trong việc xử lý các biến đổi văn bản như loại bỏ dấu ngoặc. Chúng là công cụ không thể thiếu trong bộ dụng cụ của bất kỳ người viết kịch bản vỏ nào.

Bash kể từ đó đã phát triển và việc thay thế biến thêm một tầng đơn giản cho các thao tác chuỗi quy mô nhỏ. Nó giúp bạn không phải đưa ra lệnh cho các nhị phân bên ngoài, làm cho các kịch bản của bạn hiệu quả hơn một chút.

Trong khi `tr` tuyệt vời cho việc xóa ký tự, nó không xử lý được các mẫu phức tạp. `sed`, mặt khác, sử dụng biểu thức chính quy, vì vậy đôi khi nó quá mạnh và có thể chậm hơn cho các thao tác đơn giản.

Việc chọn lựa giữa những phương pháp này phụ thuộc vào trường hợp cụ thể của bạn. Nếu bạn cần loại bỏ nhiều loại dấu ngoặc và bạn đang trong bối cảnh của một kịch bản Bash, việc sử dụng thay thế biến là một lựa chọn không cần suy nghĩ vì tính đơn giản của nó. Nhưng nếu bạn đang biến đổi dòng văn bản hoặc dữ liệu nhiều dòng, `tr` và `sed` là những người bạn đồng hành hàng đầu của bạn.

## Xem thêm:
- Hướng dẫn Bash của GNU, đặc biệt là các phần về Mở rộng Tham số và Mở rộng Tham số Shell: https://www.gnu.org/software/bash/manual/
- Hướng dẫn về lệnh `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Tổng quan về trình chỉnh sửa dòng `sed`: https://www.gnu.org/software/sed/manual/sed.html

---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases: - /vi/bash/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:17.682458-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Chuyển đổi Chuỗi sang Chữ thường trong Bash

### Cái gì & Tại sao?

Biến đổi chuỗi chữ thường là quá trình chuyển tất cả các ký tự chữ cái trong một chuỗi sang dạng chữ thường. Các lập trình viên biến đổi chuỗi sang chữ thường để đạt được sự nhất quán, so sánh không phân biệt chữ hoa chữ thường, và để đáp ứng yêu cầu của hệ thống hoặc ứng dụng.

### Làm thế nào:

Dưới đây là cách đơn giản để chuyển đổi một chuỗi sang chữ thường trong Bash:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

Đầu ra:

```
make me lower case
```

Bash 4.0 trở lên có cách tích hợp sẵn với phép mở rộng tham số:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

Đầu ra:

```
make me lower case
```

### Sâu hơn:

Trước Bash 4.0, các phương pháp thường được sử dụng để chuyển đổi chuỗi sang chữ thường bao gồm việc sử dụng các tiện ích bên ngoài như `tr`, `awk`, hoặc `sed`. Mỗi tiện ích này cung cấp các cách khác nhau để thao tác với chuỗi ngoài việc chỉ thay đổi chữ hoa thành chữ thường, nhưng có thể cần khởi động một quá trình mới, ảnh hưởng đến hiệu suất.

Sự giới thiệu cú pháp `${parameter,,pattern}` trong Bash 4.0 cung cấp một tính năng bản địa để biến đổi chuỗi, nhanh chóng hơn và không phụ thuộc vào các tiện ích bên ngoài. Có các phương án thay thế trong chính Bash:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - như đã được trình bày ở trên.

Về mặt triển khai, `${parameter,,pattern}` không chỉ thay đổi các ký tự ASCII; chúng còn nhận biết UTF-8 và có thể xử lý các ký tự không phải tiếng Anh, làm cho chúng linh hoạt cho các ứng dụng quốc tế.

### Xem thêm

- Mở rộng tham số Bash: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Lệnh `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Lập trình AWK: https://www.gnu.org/software/gawk/manual/gawk.html
- Trình biên tập dòng `sed`: https://www.gnu.org/software/sed/manual/sed.html

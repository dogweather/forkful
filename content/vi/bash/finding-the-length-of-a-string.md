---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- vi/bash/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:28.799151-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Việc tìm chiều dài của một chuỗi nghĩa là đếm số lượng ký tự của nó. Các lập trình viên làm điều này để xác thực đầu vào, lặp qua các ký tự, hoặc đơn giản là để căn chỉnh đầu ra.

## Cách Thực Hiện:
Biểu tượng `#` có thể giúp bạn hoàn thành công việc trong bash. Sử dụng nó với sự mở rộng tham số. Dưới đây là cách làm:

```bash
my_string="Hello, World!"
string_length=${#my_string}
echo $string_length
```

Mẫu đầu ra:

```
13
```

## Sâu Hơn Nữa
Ngày xưa, mọi người sử dụng `expr` hoặc các công cụ bên ngoài như `wc -m`, nhưng các tính năng đã tích hợp sẵn trong Bash đã thay đổi cách chơi. Cú pháp `${#var}` là một phần của sự mở rộng tham số được giới thiệu trong Bash. Nó nhanh chóng và hiệu quả vì không phải tạo một tiểu trình con hay gọi một chương trình bên ngoài.

Có lựa chọn khác? Chắc chắn rồi:

- `expr length "$my_string"` cho bạn kết quả tương tự, nhưng nó hơi cũ kỹ.
- `echo -n $my_string | wc -m` sử dụng `wc` để đếm, nhưng nó quá mức cần thiết cho những tác vụ đơn giản.

Những chi tiết... Khi bạn sử dụng `${#my_string}`, nó mặc định cho bạn chiều dài tính bằng byte. Nếu văn bản của bạn đi trên con đường của unicode, bạn có thể cần phải xem xét đến các ký tự đa byte. Đó là khi mọi thứ trở nên phức tạp hơn.

## Xem Thêm
Lặn sâu vào các trang man với `man bash` để tìm hiểu kỹ hơn về sự mở rộng tham số. Đối với những ai muốn xử lý chuỗi ngoài ASCII cơ bản, Hướng Dẫn Scripting Bash Nâng Cao cung cấp một số cái nhìn: https://www.tldp.org/LDP/abs/html/. Và vì tình yêu với việc học hỏi, hãy theo dõi https://www.gnu.org/software/bash/manual/ để có thông tin mới nhất về Bash.

---
title:                "Nối chuỗi ký tự"
aliases:
- vi/bash/concatenating-strings.md
date:                  2024-01-28T21:57:10.382100-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi trong Bash có nghĩa là ghép hai hoặc nhiều đoạn văn bản lại với nhau. Lập trình viên thực hiện điều đó để xây dựng các lệnh, tạo đường dẫn tệp, hoặc đơn giản là định dạng văn bản đầu ra.

## Cách thực hiện:

Dưới đây là cách nhanh chóng để nối các chuỗi của bạn trong Bash:

```Bash
# Nối bằng cách đặt các chuỗi cạnh nhau
greeting="Xin chào, "
name="thế giới!"
welcome=$greeting$name
echo $welcome  # Đầu ra: Xin chào, thế giới!

# Sử dụng dấu ngoặc nhọn để làm rõ
version="phiên bản"
number=1
full_version=${version}_$number
echo $full_version  # Đầu ra: phiên bản_1

# Nối với biến và ký tự
timestamp=$(date +%Y%m%d)  # Lấy ngày hiện tại theo định dạng YYYYMMDD
filename="sao_lưu_${timestamp}.tar.gz"
echo $filename  # Đầu ra: sao_lưu_20230315.tar.gz
```

## Đào sâu hơn

Trở lại những ngày trước khi giao diện người dùng đồ họa thống trị, dòng lệnh và kịch bản là vua của sự tương tác máy tính. Việc nối chuỗi luôn là thiết yếu bởi vì nó cho phép thao tác lệnh và tệp linh hoạt.

Một phương thức thay thế lịch sử là lệnh `expr`, giờ đây cảm thấy như một cổ vật:

```Bash
older_way=$(expr $greeting $name)
```

Nhưng Bash nói, "Ai cần phải vất vả như vậy?" và làm cho nó trở nên tự nhiên. Thế nào? Vâng, Bash xử lý các chuỗi như những người bạn ấm áp vậy: đặt chúng cạnh nhau và chúng sẽ ôm nhau thành một chuỗi dài.

Bên dưới lớp vỏ, Bash xử lý điều này mà không cần bất kỳ chức năng hoặc cú pháp đặc biệt nào cho việc nối. Các từ hoặc biến chỉ đơn giản chảy vào nhau. Tuy nhiên, nếu biến của bạn có thể bắt đầu với một số hoặc một dấu gạch dưới, bạn thường sẽ bọc chúng trong dấu ngoặc nhọn để tránh nhầm lẫn với tên biến khác.

Có một điều cần lưu ý: khoảng trắng quan trọng. Nếu bạn không cẩn thận, bạn có thể kết thúc với những khoảng cách không mong muốn hoặc một lộn xộn đè nén.

Một phương thức thay thế hiện tại là sử dụng hàm `printf`, cung cấp cho bạn nhiều kiểm soát hơn về định dạng:

```Bash
printf -v full_greeting "%s%s" "$greeting" "$name"
echo $full_greeting  # Đầu ra: Xin chào, thế giới!
```

## Xem thêm

- [Hướng dẫn GNU Bash](https://www.gnu.org/software/bash/manual/) cho tất cả mọi thứ về Bash.
- [Hướng dẫn Lập trình nâng cao với Bash-Scripting](https://tldp.org/LDP/abs/html/) cho các bài tập và nhiều ví dụ hơn.

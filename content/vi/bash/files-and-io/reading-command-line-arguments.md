---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:22.981989-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EA7u ra m\u1EABu gi\u1EA3 s\u1EED k\u1ECB\
  ch b\u1EA3n c\u1EE7a b\u1EA1n \u0111\u01B0\u1EE3c \u0111\u1EB7t t\xEAn l\xE0 'example.sh'\
  \ v\xE0 b\u1EA1n g\u1ECDi `./example.sh arg1 arg2`."
lastmod: '2024-04-05T21:53:38.267568-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ra m\u1EABu gi\u1EA3 s\u1EED k\u1ECBch b\u1EA3n c\u1EE7a b\u1EA1\
  n \u0111\u01B0\u1EE3c \u0111\u1EB7t t\xEAn l\xE0 'example.sh' v\xE0 b\u1EA1n g\u1ECD\
  i `./example.sh arg1 arg2`."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
```Bash
#!/bin/bash

# In ra tên của kịch bản.
echo "Tên kịch bản: $0"

# In ra đối số đầu tiên.
echo "Đối số đầu tiên: $1"

# In ra tất cả các đối số.
echo "Tất cả các đối số: $@"
```

Đầu ra mẫu giả sử kịch bản của bạn được đặt tên là 'example.sh' và bạn gọi `./example.sh arg1 arg2`:

```
Tên kịch bản: ./example.sh
Đối số đầu tiên: arg1
Tất cả các đối số: arg1 arg2
```

Lặp qua các đối số:

```Bash
#!/bin/bash

# Lặp qua từng đối số.
for arg in "$@"; do
  echo "Đối số: $arg"
done
```

## Đào sâu
Bash đã hỗ trợ các đối số dòng lệnh từ rất lâu; chúng là các tham số vị trí, `$0` đến `$9`, với `$@` và `$*` hiển thị tất cả. `$0` là chính kịch bản đó, `$1` đến `$9` là đối số thứ nhất đến thứ chín; cần sử dụng ngoặc như `${10}` cho đối số thứ mười trở đi.

Sử dụng `$@` thường tốt hơn là `$*` vì nó xử lý chính xác các đối số chứa khoảng trắng. `$@` cung cấp mỗi đối số như một "từ" riêng biệt; `$*` kết hợp tất cả chúng thành một "từ" duy nhất.

Bạn có thể chuyển qua các đối số bằng cách sử dụng lệnh `shift`, đẩy `$2` lên thành `$1`, và tiếp tục như vậy, loại bỏ `$1` cũ.

Có phương án thay thế? Chắc chắn rồi. `getopts` và `getopt` cung cấp nhiều kiểm soát hơn cho các tùy chọn (như -h cho trợ giúp) và phân tích cờ; hãy kiểm tra chúng nếu `$1`, `$2`,... không đáp ứng được nhu cầu của bạn.

## Xem Thêm
- Tài liệu Bash về các Tham số Đặc biệt: https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html
- Hướng dẫn Lập trình Bash Nâng cao: https://www.tldp.org/LDP/abs/html/
- Hướng dẫn `getopts`: https://wiki.bash-hackers.org/howto/getopts_tutorial

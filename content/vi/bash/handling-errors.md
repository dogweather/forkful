---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:02:16.496139-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xử lý lỗi trong kịch bản Bash là việc lường trước những tình huống có thể đi sai và xử lý chúng một cách nhẹ nhàng. Tại sao ư? Chà, nó giữ cho kịch bản của bạn vững chãi và ngăn người dùng phải bối rối khi mọi thứ không hoạt động như dự kiến.

## Làm thế nào:

```Bash
#!/bin/bash

# Chuyển hướng stderr vào một tệp
grep "something" file.txt 2> errors.log

# Xử lý lỗi với trạng thái thoát
if ! grep "something" file.txt; then
    echo "Ôi, có gì đó không đúng khi tìm kiếm 'something'."
    exit 1
fi

# Sử dụng trap để dọn dẹp trước khi thoát do lỗi
cleanup() {
  echo "Đang dọn dẹp tệp tạm..."
  rm temp_*
}

trap cleanup ERR

# lỗi chủ ý: tệp không tồn tại
cat temp_file.txt
```

Kết quả mẫu khi xảy ra lỗi:

```
Đang dọn dẹp tệp tạm...
cat: temp_file.txt: Không có tệp hoặc thư mục nào như vậy
```

## Sâu hơn

Xử lý lỗi trong kịch bản Bash có từ nguồn gốc của vỏ Unix, nơi mà những kịch bản vững chãi và đáng tin cậy là (và vẫn là) thiết yếu cho quản trị hệ thống và tự động hóa. Theo truyền thống, lỗi trong Bash được xử lý bằng cách kiểm tra trạng thái thoát của một lệnh, quy ước trả về 0 cho thành công và một giá trị khác 0 cho thất bại.

Bash giới thiệu lệnh `trap` như một tính năng tích hợp, cho phép người dùng chỉ định các lệnh để chạy trên các tín hiệu hoặc thoát kịch bản. Điều này hữu ích cho các tác vụ dọn dẹp hoặc cơ chế xử lý lỗi cuối cùng.

Cũng có lệnh `set`, có thể thay đổi hành vi của Bash khi có lỗi. Ví dụ, `set -e` sẽ khiến một kịch bản thoát ngay lập tức nếu bất kỳ lệnh nào thoát với trạng thái không phải là 0, một cách để thất bại nhanh chóng và tránh lỗi lan rộng.

Các phương thức thay thế cho việc xử lý lỗi tích hợp của Bash bao gồm việc kiểm tra rõ ràng sự tồn tại của các tệp, sử dụng thay thế lệnh, hoặc thậm chí viết chức năng của riêng bạn để xử lý lỗi một cách tinh tế hơn.

Mặc dù việc xử lý lỗi một cách nghiêm ngặt đôi khi cảm thấy quá mức cho các kịch bản nhỏ, đó là một thực hành có thể tiết kiệm rất nhiều thời gian gỡ lỗi và ngăn chặn hành vi không mong đợi cho cả bạn và người dùng.

## Xem thêm

- Bash Manual về Các Tham Số Shell: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Phần về Xử lý Lỗi trong Hướng dẫn Lập Kịch bản Bash Nâng cao: https://www.tldp.org/LDP/abs/html/exit-status.html
- Hướng dẫn sâu về `trap`: https://mywiki.wooledge.org/SignalTrap

Nhớ rằng, lập kịch bản là một hình thức nghệ thuật, và cách bạn xử lý những sai lệch và vấp ngã có thể làm cho kiệt tác của bạn mạnh mẽ hơn. Chúc bạn lập kịch bản vui vẻ!

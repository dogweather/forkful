---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:14.153857-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Phân chia mã thành các hàm có nghĩa là chia nhỏ script thành các khối nhỏ hơn, có thể sử dụng lại được và thực hiện các nhiệm vụ cụ thể. Điều này làm cho mã nguồn trở nên sạch sẽ hơn, dễ hiểu hơn và dễ gỡ lỗi hơn.

## Cách thực hiện:
Tạo một hàm đơn giản trong Bash:

```Bash
greet() {
  echo "Xin chào, $1!"
}
```

Sử dụng bằng cách gọi hàm với một tham số:

```Bash
greet "Thế giới"  # Đầu ra: Xin chào, Thế giới!
```

Hàm có thể trả về giá trị sử dụng `return` cho các mã trạng thái số (không dùng cho việc trả về dữ liệu thực sự):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # Đầu ra: 7
```

Lưu ý rằng `$?` ghi lại giá trị trả về của lệnh cuối cùng, là kết quả số của hàm `add`.

## Sâu hơn
Trong Bash, hàm đã là cách để phân chia mã nguồn kể từ những phiên bản đầu tiên. Lịch sử, sử dụng hàm phù hợp với các nguyên tắc lập trình cấu trúc được giới thiệu vào những năm 1960 để cải thiện chất lượng mã nguồn.

Các phương án khác thay thế cho hàm bao gồm việc tải các tệp script hoặc sử dụng các bí danh, nhưng chúng không cung cấp cùng một mức độ module và tái sử dụng.

Một chi tiết thực hiện đáng chú ý trong Bash là hàm được coi là công dân hạng nhất; chúng không cần có từ khóa khai báo cụ thể như `function` trong các ngôn ngữ khác, mặc dù `function` là tùy chọn trong Bash để dễ đọc hơn. Phạm vi của hàm cũng thú vị—các biến mặc định là toàn cục trừ khi được khai báo là cục bộ, điều này có thể dẫn đến hành vi không mong muốn nếu không được quản lý đúng cách.

## Xem thêm
- Hướng dẫn Bash về Hàm Shell: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Hướng dẫn Nâng cao về Scripting Bash: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" cho các khái niệm và thực hành viết kịch bản hàm sâu hơn.

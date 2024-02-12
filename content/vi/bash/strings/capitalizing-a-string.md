---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:45.100648-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Chuyển chữ cái đầu tiên của mỗi từ trong chuỗi thành chữ in hoa được gọi là việc viết hoa chuỗi. Lập trình viên làm điều này để định dạng, đồng nhất, và tăng tính đọc được, đặc biệt trong tiêu đề hoặc phần đầu.

## Làm thế nào:

Trong Bash, bạn có thể viết hoa chuỗi theo nhiều cách. Dưới đây là một cách truyền thống sử dụng `awk`:

```Bash
echo "hello world" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1'
```

Kết quả:
```
Hello World
```

Hoặc, chỉ sử dụng Bash:

```Bash
string="hello world"
capitalize() {
  echo "$1" | while IFS=" " read -r word; do 
    echo -n "${word^} " 
  done
  echo
}
capitalize "$string"
```

Kết quả:
```
Hello World 
```

## Đào sâu:

Ngày trước, ‘awk’ là công cụ được ưa chuộng để thao tác với văn bản. Nó mạnh mẽ nhưng kém trực quan đối với người mới bắt đầu. Với sự phát triển của Bash, đặc biệt từ phiên bản 4 trở đi, các khả năng như thao tác chuỗi đã được cải thiện.

Cách sử dụng `awk` là cổ điển, lặp qua từng từ và viết hoa chữ cái đầu tiên. Bash thuần tuý sử dụng sự mở rộng tham số: `${word^}` viết hoa chữ cái đầu tiên của `$word`. Sự mở rộng tham số trực tiếp và nhanh chóng, giảm bớt số lượng công cụ bên ngoài cần thiết.

Tại sao điều này lại quan trọng? Vâng, viết hoa chuỗi là một nhu cầu phổ biến trong các tác vụ lập trình. Việc viết hoa đúng cách có thể rất quan trọng đối với giao diện người dùng hay xử lý dữ liệu nơi mà cách trình bày là quan trọng. Biết cách làm điều này trong shell của bạn có thể giải cứu bạn.

## Xem thêm:

- Hướng dẫn Bash về sự mở rộng tham số: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Giới thiệu và sử dụng `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- Thảo luận trên StackOverflow về thao tác văn bản trong Bash: https://stackoverflow.com/questions/tagged/bash+string+capitalization

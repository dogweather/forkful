---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:45.100648-07:00
description: "Chuy\u1EC3n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED7i\
  \ t\u1EEB trong chu\u1ED7i th\xE0nh ch\u1EEF in hoa \u0111\u01B0\u1EE3c g\u1ECD\
  i l\xE0 vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng, \u0111\u1ED3ng nh\u1EA5t,\
  \ v\xE0\u2026"
lastmod: '2024-02-25T18:49:35.198096-07:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED7i t\u1EEB\
  \ trong chu\u1ED7i th\xE0nh ch\u1EEF in hoa \u0111\u01B0\u1EE3c g\u1ECDi l\xE0 vi\u1EC7\
  c vi\u1EBFt hoa chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng, \u0111\u1ED3ng nh\u1EA5t, v\xE0\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:45.100648-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Bash, b\u1EA1n c\xF3 th\u1EC3 vi\u1EBF\
  t hoa chu\u1ED7i theo nhi\u1EC1u c\xE1ch. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t c\xE1ch truy\u1EC1n th\u1ED1ng s\u1EED d\u1EE5ng `awk`."
lastmod: '2024-03-13T22:44:36.854768-06:00'
model: gpt-4-0125-preview
summary: "Trong Bash, b\u1EA1n c\xF3 th\u1EC3 vi\u1EBFt hoa chu\u1ED7i theo nhi\u1EC1\
  u c\xE1ch."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

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

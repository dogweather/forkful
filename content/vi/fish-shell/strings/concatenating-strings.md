---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.792053-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Fish, gh\xE9p c\xE1c chu\u1ED7i l\u1EA1\
  i v\u1EDBi nhau b\u1EB1ng kho\u1EA3ng tr\u1EAFng ho\u1EB7c s\u1EED d\u1EE5ng `string\
  \ join`."
lastmod: '2024-03-13T22:44:37.197297-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, gh\xE9p c\xE1c chu\u1ED7i l\u1EA1i v\u1EDBi nhau b\u1EB1ng kho\u1EA3\
  ng tr\u1EAFng ho\u1EB7c s\u1EED d\u1EE5ng `string join`."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
Trong Fish, ghép các chuỗi lại với nhau bằng khoảng trắng hoặc sử dụng `string join`.

```fish
# Kết hợp 'Hello' và 'World!' với một khoảng trắng
echo 'Hello' 'World!'

# Đầu ra: Hello World!

# Nối các biến
set greet "Howdy"
set who "Partner"
echo $greet $who

# Đầu ra: Howdy Partner

# Nối không khoảng trắng với string join
set file "report"
set ext "txt"
string join '' $file '.' $ext

# Đầu ra: report.txt
```

## Sâu hơn
Việc nối chuỗi đã tồn tại từ bình minh của lập trình. Trong Fish, `string join` sạch sẽ hơn so với các phương pháp cũ, chẳng hạn sử dụng `echo` theo sau là các biến chuỗi không có dấu ngoặc. Cách tiếp cận này tránh được gánh nặng của subcommand, có thể là một lợi ích về hiệu suất.

Các phương án thay thế bao gồm việc sử dụng `printf`, mang lại nhiều kiểm soát định dạng hơn nhưng phức tạp hơn một chút đối với các hoạt động nối đơn giản. Ví dụ:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

Lệnh `string` của Fish là một phần của bộ công cụ xử lý chuỗi tích hợp được giới thiệu để làm cho việc xử lý văn bản trở nên đơn giản hơn. Nó không độc quyền với Fish, nhưng sự bao gồm nó như một công cụ tích hợp giúp mọi thứ trở nên đơn giản.

## Xem thêm
- Tài liệu chính thức của Fish: [link](https://fishshell.com/docs/current/cmds/string.html)
- Hướng dẫn của cộng đồng: [link](https://fishshell.com/docs/current/tutorial.html#tutorial)
- Thảo luận về xử lý chuỗi trong shells: [link](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)

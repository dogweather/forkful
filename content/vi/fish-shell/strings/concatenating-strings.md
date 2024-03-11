---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.792053-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. L\u1EADp tr\xECnh\
  \ vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 k\u1EBFt h\u1EE3\
  p v\u0103n b\u1EA3n, nh\u01B0 x\xE2y d\u1EF1ng m\u1ED9t c\xE2u ho\xE0n ch\u1EC9\
  nh t\u1EEB\u2026"
lastmod: '2024-03-11T00:14:10.506460-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1i v\u1EDB\
  i nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. L\u1EADp tr\xECnh vi\xEAn\
  \ th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 k\u1EBFt h\u1EE3p v\u0103\
  n b\u1EA3n, nh\u01B0 x\xE2y d\u1EF1ng m\u1ED9t c\xE2u ho\xE0n ch\u1EC9nh t\u1EEB\
  \u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối. Lập trình viên thực hiện điều này để kết hợp văn bản, như xây dựng một câu hoàn chỉnh từ các từ hoặc tạo đường dẫn tệp.

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

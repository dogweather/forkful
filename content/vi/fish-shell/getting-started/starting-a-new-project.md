---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:37.705860-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 kh\u1EDFi t\u1EA1o m\u1ED9t th\u01B0 m\u1EE5c m\u1EDBi ho\xE0n to\xE0n v\u1EDB\
  i m\u1ECDi th\u1EE9 b\u1EA1n c\u1EA7n \u0111\u1EC3 b\u1EAFt \u0111\u1EA7u vi\u1EBF\
  t code. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ kh\u1EDFi \u0111\u1EA7u\u2026"
lastmod: '2024-03-13T22:44:37.211572-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129a\
  \ l\xE0 kh\u1EDFi t\u1EA1o m\u1ED9t th\u01B0 m\u1EE5c m\u1EDBi ho\xE0n to\xE0n v\u1EDB\
  i m\u1ECDi th\u1EE9 b\u1EA1n c\u1EA7n \u0111\u1EC3 b\u1EAFt \u0111\u1EA7u vi\u1EBF\
  t code. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ kh\u1EDFi \u0111\u1EA7u\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cái gì và Tại sao?
Bắt đầu một dự án mới có nghĩa là khởi tạo một thư mục mới hoàn toàn với mọi thứ bạn cần để bắt đầu viết code. Các lập trình viên làm điều này để khởi đầu phát triển theo cách sạch sẽ, có tổ chức.

## Làm thế nào:
```fish
# Tạo một thư mục mới và vào thư mục đó
mkdir my_fish_project
cd my_fish_project

# Khởi tạo một kho lưu trữ git
git init

# Tạo một commit ban đầu với file .gitignore
echo "*.log" > .gitignore
git add .gitignore
git commit -m "Commit ban đầu với .gitignore"

# Bonus: Thiết lập môi trường ảo nếu có thể (không phải bản địa của Fish hay git)
# Đảm bảo đã cài đặt công cụ môi trường ảo.
```
Kết quả mẫu:
```
Đã khởi tạo kho Git trống tại /path/to/my_fish_project/.git/
[master (root-commit) abc1234] Commit ban đầu với .gitignore
 1 file thay đổi, 1 chèn thêm(+)
 tạo chế độ 100644 .gitignore
```

## Sâu hơn nữa
Việc thiết lập một dự án mới có một lịch sử dài, trở nên tiêu chuẩn hóa hơn với sự phát triển của các hệ thống kiểm soát phiên bản hiện đại như Git. Mặc dù một số người có thể sử dụng những phương pháp đồ họa, những người yêu thích dòng lệnh thì ưu tiên sự kiểm soát tỉ mỉ và tốc độ của các lệnh terminal. Fish Shell, nổi tiếng với thiết kế thân thiện với người dùng, làm cho việc này đơn giản hơn với các tính năng hữu ích như làm nổi bật cú pháp và tự động hoàn thành lệnh.

Các phương án thay thế bao gồm việc sử dụng các IDE có tích hợp khởi tạo dự án hoặc scripts trong các shell khác như Bash hay Zsh — nhưng Fish nổi bật với sự đơn giản và tương tác của nó. Khi đến với thực thi, quá trình khởi tạo về bản chất có thể tùy chỉnh; bạn thích nghi nó để phù hợp với bộ công cụ và chuỗi công cụ của mình. Dù là thêm công cụ xây dựng, thiết lập linters, hay tạo cấu trúc thư mục, tất cả đều vì làm cho quá trình phát triển tương lai của bạn trở nên mượt mà hơn.

## Xem thêm
- Tài liệu về Fish Shell: https://fishshell.com/docs/current/index.html
- Cơ bản về Git: https://git-scm.com/book/en/v2/Getting-Started-Git-Basics
- Thiết lập Môi trường Ảo: https://virtualfish.readthedocs.io/en/latest/index.html

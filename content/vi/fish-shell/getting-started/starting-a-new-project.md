---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:37.705860-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t d\u1EF1\
  \ \xE1n m\u1EDBi c\xF3 m\u1ED9t l\u1ECBch s\u1EED d\xE0i, tr\u1EDF n\xEAn ti\xEA\
  u chu\u1EA9n h\xF3a h\u01A1n v\u1EDBi s\u1EF1 ph\xE1t tri\u1EC3n c\u1EE7a c\xE1\
  c h\u1EC7 th\u1ED1ng ki\u1EC3m so\xE1t phi\xEAn b\u1EA3n hi\u1EC7n \u0111\u1EA1\
  i\u2026"
lastmod: '2024-04-05T22:50:51.508359-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 m\u1ED9\
  t l\u1ECBch s\u1EED d\xE0i, tr\u1EDF n\xEAn ti\xEAu chu\u1EA9n h\xF3a h\u01A1n v\u1EDB\
  i s\u1EF1 ph\xE1t tri\u1EC3n c\u1EE7a c\xE1c h\u1EC7 th\u1ED1ng ki\u1EC3m so\xE1\
  t phi\xEAn b\u1EA3n hi\u1EC7n \u0111\u1EA1i nh\u01B0 Git."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

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

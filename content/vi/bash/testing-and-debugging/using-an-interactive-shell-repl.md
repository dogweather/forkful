---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:14.423441-07:00
description: "C\xE1ch th\u1EE9c: Trong Bash, terminal c\u1EE7a b\u1EA1n v\u1EC1 c\u01A1\
  \ b\u1EA3n l\xE0 m\u1ED9t REPL. B\u1EA1n g\xF5 m\u1ED9t l\u1EC7nh; n\xF3 \u0111\u1ECD\
  c, \u0111\xE1nh gi\xE1, in k\u1EBFt qu\u1EA3, v\xE0 l\u1EB7p l\u1EA1i ch\u1EDD l\u1EC7\
  nh ti\u1EBFp theo c\u1EE7a b\u1EA1n. D\u01B0\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.881364-06:00'
model: gpt-4-0125-preview
summary: "Trong Bash, terminal c\u1EE7a b\u1EA1n v\u1EC1 c\u01A1 b\u1EA3n l\xE0 m\u1ED9\
  t REPL."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cách thức:
Trong Bash, terminal của bạn về cơ bản là một REPL. Bạn gõ một lệnh; nó đọc, đánh giá, in kết quả, và lặp lại chờ lệnh tiếp theo của bạn. Dưới đây là một ví dụ về việc sử dụng Bash như một REPL:

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

Nhập của bạn đằng sau dấu nhắc `$ `, với đầu ra được in trên dòng tiếp theo. Đơn giản, phải không?

## Sâu hơn
Bash, viết tắt của Bourne Again SHell, là shell mặc định trên nhiều hệ thống dựa trên Unix. Đây là một bản nâng cấp của Bourne shell gốc, được xây dựng vào cuối những năm 1970. Mặc dù Bash là một công cụ lập kịch bản mạnh mẽ, chế độ tương tác của nó cho phép bạn thực thi các lệnh từng dòng một.

Khi xem xét các lựa chọn thay thế, bạn có REPL của Python (chỉ cần gõ `python` trong terminal của bạn), Node.js (với `node`), và IPython, một shell Python tương tác nâng cao. Mỗi ngôn ngữ có xu hướng có triển khai REPL của riêng mình.

Bên dưới, REPL là các vòng lặp phân tích cú pháp đầu vào của bạn (lệnh hoặc mã), chạy nó và trả kết quả về stdout (màn hình của bạn), thường sử dụng trực tiếp trình thông dịch của ngôn ngữ đó. Sự ngay lập tức của phản hồi này rất tuyệt vời cho việc học và tạo mẫu.

## Xem Thêm
- [Tài liệu chính thức GNU Bash](https://gnu.org/software/bash/manual/bash.html)
- [Hướng dẫn tương tác học Shell](https://www.learnshell.org/)
- [Trang Web Chính Thức của IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): Một REPL trực tuyến đa ngôn ngữ (Không chỉ Bash!)

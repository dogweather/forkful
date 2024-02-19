---
aliases:
- /vi/bash/using-an-interactive-shell-repl/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:14.423441-07:00
description: "REPL vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop, m\u1ED9t m\xF4\
  i tr\u01B0\u1EDDng l\u1EADp tr\xECnh m\xE1y t\xEDnh \u0111\u01A1n gi\u1EA3n, t\u01B0\
  \u01A1ng t\xE1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\
  \u1EC3 nhanh ch\xF3ng vi\u1EBFt v\xE0 ki\u1EC3m\u2026"
lastmod: 2024-02-18 23:08:50.887357
model: gpt-4-0125-preview
summary: "REPL vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop, m\u1ED9t m\xF4i tr\u01B0\
  \u1EDDng l\u1EADp tr\xECnh m\xE1y t\xEDnh \u0111\u01A1n gi\u1EA3n, t\u01B0\u01A1\
  ng t\xE1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3\
  \ nhanh ch\xF3ng vi\u1EBFt v\xE0 ki\u1EC3m\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
REPL viết tắt của Read-Eval-Print Loop, một môi trường lập trình máy tính đơn giản, tương tác. Các lập trình viên sử dụng nó để nhanh chóng viết và kiểm tra mã, thử nghiệm với cú pháp, và học các khái niệm lập trình mà không cần tốn công sức tạo và chạy cả ứng dụng.

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

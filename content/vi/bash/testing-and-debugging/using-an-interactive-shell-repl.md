---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases: - /vi/bash/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:14.423441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

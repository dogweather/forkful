---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:30.503200-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EA7u ra cho `cat hello.txt`."
lastmod: '2024-04-05T21:53:38.270368-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ra cho `cat hello.txt`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
```Bash
# Tạo một tệp văn bản mới với lệnh 'echo'
echo "Xin chào, Thế giới!" > hello.txt

# Thêm nhiều dòng văn bản vào một tệp đã tồn tại với toán tử '>>'
echo "Một dòng văn bản khác." >> hello.txt

# Viết nhiều dòng sử dụng heredoc
cat << EOF > hello_multiline.txt
Xin chào, đây là dòng đầu tiên.
Và đây là dòng thứ hai.
EOF
```

Đầu ra cho `cat hello.txt`:
```
Xin chào, Thế giới!
Một dòng văn bản khác.
```

Đầu ra cho `cat hello_multiline.txt`:
```
Xin chào, đây là dòng đầu tiên.
Và đây là dòng thứ hai.
```

## Nghiên cứu sâu
Lập trình trong Shell đã là một phần cốt lõi của các hệ thống giống Unix từ những năm 1970, với `sh` (Bourne shell) là bản gốc. Ngày nay, `bash` (Bourne Again SHell) là một shell được sử dụng rộng rãi và phổ biến. Trong khi `echo` và chuyển hướng đầu ra (`>`, `>>`) là các phương pháp phổ biến để viết tệp, các phương pháp khác như `printf` cung cấp khả năng định dạng. Việc viết tệp trong kịch bản bash sử dụng bộ chỉ số tệp; `1` cho `stdout`, và việc thêm vào (`>>`) tránh được việc ghi đè tệp bằng cách sử dụng bộ chỉ số tệp `2`.

## Xem thêm
- [Hướng dẫn Bash của GNU](https://www.gnu.org/software/bash/manual/bash.html)
- [Hướng dẫn Nâng cao về Kịch bản Bash](https://www.tldp.org/LDP/abs/html/)
- [Hướng dẫn Lập trình Shell Script](https://www.shellscript.sh/)

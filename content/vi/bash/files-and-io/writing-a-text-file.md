---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:30.503200-07:00
description: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 qu\xE1\
  \ tr\xECnh l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p d\u01B0\u1EDB\
  i d\u1EA1ng v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 l\u01B0u tr\u1EEF c\u1EA5u h\xECnh, nh\u1EADt k\xFD, m\xE3\
  \u2026"
lastmod: '2024-03-11T00:14:10.195006-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 qu\xE1 tr\xEC\
  nh l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p d\u01B0\u1EDBi d\u1EA1\
  ng v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0\
  y \u0111\u1EC3 l\u01B0u tr\u1EEF c\u1EA5u h\xECnh, nh\u1EADt k\xFD, m\xE3\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc tạo một tệp văn bản là quá trình lưu trữ dữ liệu vào một tệp dưới dạng văn bản. Lập trình viên thực hiện việc này để lưu trữ cấu hình, nhật ký, mã lệnh, hoặc bất kỳ dữ liệu nào cần được tham chiếu hoặc bảo lưu theo thời gian.

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

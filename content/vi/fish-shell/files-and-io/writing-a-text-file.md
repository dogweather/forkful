---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.551472-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 vi\u1EBFt v\xE0o m\u1ED9t\
  \ t\u1EADp tin v\u0103n b\u1EA3n trong Fish, s\u1EED d\u1EE5ng `echo` ho\u1EB7c\
  \ `printf` sau \u0111\xF3 l\xE0 to\xE1n t\u1EED `>` ho\u1EB7c `>>`. `>` t\u1EA1\
  o m\u1ED9t t\u1EADp tin m\u1EDBi ho\u1EB7c ghi\u2026"
lastmod: '2024-03-13T22:44:37.235659-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 vi\u1EBFt v\xE0o m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n trong\
  \ Fish, s\u1EED d\u1EE5ng `echo` ho\u1EB7c `printf` sau \u0111\xF3 l\xE0 to\xE1\
  n t\u1EED `>` ho\u1EB7c `>>`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Cách thực hiện:
Để viết vào một tập tin văn bản trong Fish, sử dụng `echo` hoặc `printf` sau đó là toán tử `>` hoặc `>>`. `>` tạo một tập tin mới hoặc ghi đè lên một tập tin đã tồn tại, trong khi `>>` thêm vào cuối tập tin.

```fish
echo "Xin chào, fish!" > hello.txt
cat hello.txt
```
Kết quả:
```
Xin chào, fish!
```

```fish
printf "Thêm dòng này nữa nhé." >> hello.txt
cat hello.txt
```
Kết quả:
```
Xin chào, fish!
Thêm dòng này nữa nhé.
```

Để viết văn bản nhiều dòng, sử dụng chuỗi đa dòng hoặc thực thi một lệnh nhiều lần:

```fish
echo "Dòng 1
Dòng 2
Dòng 3" > multiline.txt
cat multiline.txt
```
Kết quả:
```
Dòng 1
Dòng 2
Dòng 3
```

## Sâu hơn nữa
Shell Fish, ra đời từ sự không hài lòng với ngôn ngữ kịch bản của các shell hiện có, được biết đến với ngôn ngữ kịch bản thân thiện với người dùng. Khi so sánh với các shell khác, lệnh chuyển hướng của Fish tương tự như trong bash hoặc zsh, nhưng với cú pháp kịch bản nâng cao.

Các phương pháp thay thế khác để viết tập tin trực tiếp từ shell bao gồm việc sử dụng các trình soạn thảo văn bản như `vi` hoặc `nano`, hoặc ngôn ngữ kịch bản như Python hoặc Perl cho việc thao tác phức tạp hơn.

Việc hiểu cách Fish quản lý các mô tả tập tin và sự khác biệt giữa `>` (ghi đè) và `>>` (thêm vào) là rất quan trọng cho việc quản lý tập tin đúng cách.

## Xem thêm
- Tài liệu Fish về I/O Redirection: https://fishshell.com/docs/current/commands.html#redirect
- Tìm hiểu thêm về chỉnh sửa văn bản với `nano`: https://www.nano-editor.org/
- Hướng dẫn về `vi` (Vim): https://vimhelp.org/

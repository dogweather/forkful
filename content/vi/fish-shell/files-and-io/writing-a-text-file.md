---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.551472-07:00
description: "Vi\u1EBFt v\xE0o m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n c\xF3 ngh\u0129\
  a l\xE0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u nh\u01B0 v\u0103n b\u1EA3n ho\u1EB7\
  c m\xE3 tr\xEAn m\xE1y t\xEDnh c\u1EE7a b\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u tr\u1EEF c\u1EA5u h\xECnh,\u2026"
lastmod: '2024-03-13T22:44:37.235659-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt v\xE0o m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n c\xF3 ngh\u0129\
  a l\xE0 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u nh\u01B0 v\u0103n b\u1EA3n ho\u1EB7\
  c m\xE3 tr\xEAn m\xE1y t\xEDnh c\u1EE7a b\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u tr\u1EEF c\u1EA5u h\xECnh,\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Cái gì & Tại sao?
Viết vào một tập tin văn bản có nghĩa là lưu trữ dữ liệu như văn bản hoặc mã trên máy tính của bạn. Các lập trình viên làm điều này để lưu trữ cấu hình, thông tin nhật ký, hoặc lưu dữ liệu để sử dụng sau.

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

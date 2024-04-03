---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:33.339814-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Bash, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng l\u1EC7nh `date` k\xE8m theo c\u1EDD `-d` \u0111\u1EC3 thao t\xE1c v\u1EDB\
  i c\xE1c ng\xE0y. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.896545-06:00'
model: gpt-4-0125-preview
summary: "Trong Bash, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng l\u1EC7nh `date` k\xE8\
  m theo c\u1EDD `-d` \u0111\u1EC3 thao t\xE1c v\u1EDBi c\xE1c ng\xE0y."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Làm thế nào:
Trong Bash, bạn có thể sử dụng lệnh `date` kèm theo cờ `-d` để thao tác với các ngày. Dưới đây là cách làm:

```Bash
# Ngày Hiện Tại
date

# Ngày Tương Lai: 10 ngày kể từ bây giờ
date -d "+10 days"

# Ngày Quá Khứ: 10 ngày trước
date -d "-10 days"

# Ngày Tương Lai Cụ Thể: Thêm tuần, tháng, năm
date -d "+1 month"
date -d "+2 weeks"
date -d "+1 year"

# Đầu ra mẫu cho ngày tương lai
Mon 31 Jan 2023 12:34:56 PM PST
```

## Sâu hơn
Việc thao tác với các ngày là một yêu cầu phổ biến trong lập trình và viết kịch bản. Trong lịch sử, nhiệm vụ này trở nên cồng kềnh và dễ mắc lỗi hơn khi xử lý năm nhuận, múi giờ, v.v. Trong các hệ thống giống Unix, lệnh `date` đã phát triển để bao gồm các tùy chọn dễ dàng cho việc tính toán ngày.

Các phương pháp thay thế bao gồm sử dụng toán học shell hoặc các công cụ bên ngoài như `awk` hoặc `perl` cho lô-gíc ngày phức tạp hơn, nhưng lệnh `date` vẫn là cách dễ dàng và trực tiếp nhất cho các hoạt động cơ bản. Đằng sau hậu trường, lệnh `date` sử dụng các thư viện hệ thống để xử lý sự phức tạp của việc tính toán thời gian, trừu tượng hóa điều này khỏi người dùng.

## Xem Thêm
- Hướng dẫn Coreutils GNU về Ngày: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Nhiều ví dụ và trường hợp sử dụng: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Hướng dẫn Lập trình Bash Nâng Cao: https://tldp.org/LDP/abs/html/abs-guide.html

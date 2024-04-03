---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:33.339814-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 l\xE0 \u0111\u1EC3 t\xECm ra ng\xE0y tr\u01B0\u1EDB\
  c ho\u1EB7c sau m\u1ED9t kho\u1EA3ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\u2026"
lastmod: '2024-03-13T22:44:36.896545-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 l\xE0 \u0111\u1EC3 t\xECm ra ng\xE0y tr\u01B0\u1EDBc ho\u1EB7\
  c sau m\u1ED9t kho\u1EA3ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ là để tìm ra ngày trước hoặc sau một khoảng thời gian nhất định. Lập trình viên thực hiện điều này cho các nhiệm vụ như thiết lập nhắc nhở, chạy các công việc theo lịch trình, hoặc xử lý ngày hết hạn.

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

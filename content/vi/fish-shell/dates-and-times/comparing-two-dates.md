---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:34.052072-07:00
description: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra xem m\u1ED9t ng\xE0\
  y c\xF3 s\u1EDBm h\u01A1n, gi\u1ED1ng h\u1EC7t, hay mu\u1ED9n h\u01A1n ng\xE0y kia\
  \ hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 t\u1ED5 ch\u1EE9c s\u1EF1\u2026"
lastmod: '2024-02-25T18:49:35.575327-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra xem m\u1ED9t ng\xE0\
  y c\xF3 s\u1EDBm h\u01A1n, gi\u1ED1ng h\u1EC7t, hay mu\u1ED9n h\u01A1n ng\xE0y kia\
  \ hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 t\u1ED5 ch\u1EE9c s\u1EF1\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày nghĩa là kiểm tra xem một ngày có sớm hơn, giống hệt, hay muộn hơn ngày kia hay không. Lập trình viên thực hiện việc này để tổ chức sự kiện, xác nhận đầu vào, và quản lý dữ liệu nhạy cảm với thời gian.

## Làm thế nào:

Với Fish Shell, chúng ta có thể so sánh hai ngày sử dụng lệnh `date`. Dưới đây là các ví dụ.

```fish
# Lấy ngày hiện tại theo giây kể từ kỷ nguyên
set current_date (date +%s)

# Chuyển đổi một ngày cụ thể thành giây kể từ kỷ nguyên
set specific_date (date -d "2023-04-01" +%s)

# So sánh các ngày
if test $specific_date -lt $current_date
    echo "Ngày cụ thể sớm hơn ngày hiện tại."
else if test $specific_date -eq $current_date
    echo "Các ngày giống nhau."
else
    echo "Ngày cụ thể muộn hơn ngày hiện tại."
end
```
Kết quả mẫu nếu ngày hiện tại là sau ngày 1 tháng 4 năm 2023:
```
Ngày cụ thể sớm hơn ngày hiện tại.
```

## Sâu hơn nữa

Trong lịch sử, so sánh các ngày trong lập trình đã gặp một chút rắc rối do nhiều định dạng ngày và múi giờ khác nhau. Fish Shell đơn giản hóa nhiệm vụ này với hàm `date` đã tích hợp sẵn, chuyển đổi ngày thành giây kể từ kỷ nguyên Unix (1 tháng 1 năm 1970). Điều này cung cấp cho chúng ta một điểm tham chiếu vũ trụ để so sánh.

Các phương án thay thế cho Fish Shell để so sánh ngày bao gồm ngôn ngữ kịch bản như Python hoặc sử dụng công cụ thao tác `date` có sẵn trong hệ thống dựa trên Unix, như `dateutil` trong GNU core utilities (coreutils). Về cách thực hiện, khi chúng ta sử dụng `date +%s`, Fish nội bộ gọi lệnh `date` của hệ thống, đó là lý do tại sao nó hiệu quả trên các nền tảng.

So sánh ngày cũng quan trọng cho công việc cron, kịch bản sao lưu và kiểm soát truy cập dựa trên thời gian. Thành thạo so sánh ngày mang lại sự tự động hóa trơn tru hơn và ít lỗi thời gian hơn.

## Xem thêm

- [Tài liệu Fish Shell](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Thời gian Kỷ nguyên Unix](https://en.wikipedia.org/wiki/Unix_time)

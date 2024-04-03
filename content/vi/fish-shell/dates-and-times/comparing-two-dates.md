---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:34.052072-07:00
description: "L\xE0m th\u1EBF n\xE0o: V\u1EDBi Fish Shell, ch\xFAng ta c\xF3 th\u1EC3\
  \ so s\xE1nh hai ng\xE0y s\u1EED d\u1EE5ng l\u1EC7nh `date`. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1c v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:37.227525-06:00'
model: gpt-4-0125-preview
summary: "V\u1EDBi Fish Shell, ch\xFAng ta c\xF3 th\u1EC3 so s\xE1nh hai ng\xE0y s\u1EED\
  \ d\u1EE5ng l\u1EC7nh `date`."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:31.705717-07:00
description: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y tr\u01B0\u1EDBc ho\u1EB7\
  c sau m\u1ED9t kho\u1EA3ng th\u1EDDi gian \u0111\u01B0\u1EE3c x\xE1c \u0111\u1ECB\
  nh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y\u2026"
lastmod: '2024-03-13T22:44:36.115221-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y tr\u01B0\u1EDBc ho\u1EB7c sau\
  \ m\u1ED9t kho\u1EA3ng th\u1EDDi gian \u0111\u01B0\u1EE3c x\xE1c \u0111\u1ECBnh.\
  \ L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tính toán một ngày trong tương lai hoặc quá khứ nghĩa là tìm một ngày trước hoặc sau một khoảng thời gian được xác định. Lập trình viên thực hiện việc này cho các công việc như nhắc nhở, hết hạn, lập lịch hoặc các tính toán dựa trên thời gian.

## Làm thế nào:
Mô-đun `datetime` của Python giúp việc làm việc với ngày và giờ trở nên dễ dàng. Hãy xem qua:

```Python
from datetime import datetime, timedelta

# Ngày và thời gian hiện tại
now = datetime.now()
print("Bây giờ: ", now)

# Thêm 10 ngày
future_date = now + timedelta(days=10)
print("Ngày tương lai (+10 ngày): ", future_date)

# Trừ đi 5 ngày
past_date = now - timedelta(days=5)
print("Ngày quá khứ (-5 ngày): ", past_date)
```
Kết quả có thể trông như sau:
```
Bây giờ: 2023-04-01 12:34:56.789012
Ngày tương lai (+10 ngày): 2023-04-11 12:34:56.789012
Ngày quá khứ (-5 ngày): 2023-03-27 12:34:56.789012
```

Đơn giản, phải không? Chỉ cần điều chỉnh số ngày, hoặc sử dụng `weeks`, `hours`, `minutes`, hoặc `seconds` trong `timedelta` để nhảy đến thời gian bạn cần.

## Sâu hơn nữa
Ngày xưa, việc tính toán ngày và giờ là một cơn đau đầu. Bạn phải đối mặt với các năm nhuận, múi giờ, giờ ánh sáng ban ngày - một mớ bòng bong. Với `datetime` của Python và các đồng minh `date` và `time`, mọi thứ trở nên dễ dàng hơn. Mô-đun xử lý các phức tạp đằng sau hậu trường.

Bạn có thể hỏi về các phương án thay thế. Chắc chắn rồi. Thư viện như `dateutil` có thể xử lý các thao tác và phân tích ngày phức tạp hơn. Đó là sự lựa chọn khi `datetime` chưa thực sự đáp ứng được.

Về mặt triển khai, khi bạn sử dụng `timedelta`, Python điều chỉnh ngày tính toán tính đến năm nhuận và các yếu tố tương tự. Tuy nhiên, luôn kiểm tra kết quả của bạn - đặc biệt là khi xử lý với múi giờ. Và nhớ rằng, `datetime` mặc định là naif; nó không xem xét múi giờ trừ khi bạn yêu cầu.

## Xem thêm
- Tài liệu `datetime` của Python: https://docs.python.org/3/library/datetime.html
- Thư viện `dateutil`: https://dateutil.readthedocs.io/en/stable/
- Xử lý múi giờ trong Python: https://docs.python.org/3/library/zoneinfo.html

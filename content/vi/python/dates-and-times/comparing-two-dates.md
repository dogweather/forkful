---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:59.388487-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Python, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng m\xF4-\u0111un `datetime` \u0111\u1EC3 so s\xE1nh c\xE1c ng\xE0y. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.113821-06:00'
model: gpt-4-0125-preview
summary: "Trong Python, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng m\xF4-\u0111un `datetime`\
  \ \u0111\u1EC3 so s\xE1nh c\xE1c ng\xE0y."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
Trong Python, bạn có thể sử dụng mô-đun `datetime` để so sánh các ngày. Dưới đây là cách làm:

```Python
from datetime import datetime

# Định nghĩa hai ngày
date_1 = datetime(2023, 3, 25)
date_2 = datetime(2023, 4, 1)

# So sánh các ngày
print(date_1 < date_2)    # Kết quả: True
print(date_1 > date_2)    # Kết quả: False
print(date_1 == date_2)   # Kết quả: False

# Tính khoảng cách
difference = date_2 - date_1
print(difference.days)    # Kết quả: 7
```

## Sâu xa hơn
Việc so sánh các ngày không phải là điều mới. Nó đã là chìa khóa trong các hệ thống cổ xưa như chính lịch. `Datetime` của Python chỉ là việc tiếp tục truyền thống đó một cách số hóa. Có những cách khác để so sánh các ngày như sử dụng dấu thời gian Unix, hoặc thư viện như `dateutil` cho những thành tựu phức tạp. Nhưng `datetime` là cơ bản của bạn. Nó biểu diễn các ngày như những đối tượng, cho phép so sánh trực tiếp sử dụng các toán tử so sánh (`<`, `>`, `==`, v.v.). Khi bạn trừ đi các ngày, bạn nhận được một đối tượng `timedelta`, cho biết sự khác biệt về ngày, giây, và microgiây.

Ngoài ra, múi giờ có thể làm bạn bối rối. Nếu bạn đang xử lý các ngày qua các múi giờ, bạn sẽ phải làm cho chúng nhận biết được. Python cung cấp thư viện `pytz`, có thể được sử dụng với `datetime` để xử lý múi giờ hiệu quả.

## Xem thêm:
- Tài liệu mô-đun `datetime` của Python: [docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- Quản lý múi giờ: [pytz](https://pypi.org/project/pytz/)
- Thư viện `dateutil` cho việc điều chỉnh ngày phức tạp: [dateutil](https://pypi.org/project/python-dateutil/)
- Hiểu về Dấu thời gian Unix: [Thời gian Unix - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)

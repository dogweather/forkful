---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:56:59.388487-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày có nghĩa là kiểm tra ngày nào đến trước hoặc chúng cách xa nhau bao xa về thời gian. Lập trình viên làm điều này để lên lịch cho các sự kiện, đo khoảng thời gian, và sắp xếp thông tin theo trình tự thời gian.

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

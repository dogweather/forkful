---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Python gi\xFAp vi\u1EC7c chuy\u1EC3n \u0111\
  \u1ED5i ng\xE0y th\xE0nh chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. S\u1EED d\u1EE5\
  ng ph\u01B0\u01A1ng th\u1EE9c\u2026"
lastmod: '2024-04-04T02:03:05.105725-06:00'
model: gpt-4-0125-preview
summary: "Python gi\xFAp vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7\
  i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Cách thực hiện:
Python giúp việc chuyển đổi ngày thành chuỗi trở nên dễ dàng. Sử dụng phương thức [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) có sẵn trên các đối tượng [ngày](https://docs.python.org/3/library/datetime.html#date-objects). Dưới đây là cách làm:

```Python
from datetime import datetime

# Lấy ngày và thời gian hiện tại
now = datetime.now()

# Chuyển đổi thành chuỗi theo định dạng: Tháng ngày, Năm
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Đầu ra: March 29, 2023 (hoặc ngày hiện tại)

# Định dạng: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Đầu ra: 2023-03-29 (hoặc ngày hiện tại)
```


### Cách tôi thực hiện

Đây là cách tôi lấy ngày theo định dạng [ISO 8601](https://www.w3.org/QA/Tips/iso-date) với thông tin múi giờ:

```python
def datestamp() -> str:
    """ 
    Ngày và thời gian hiện tại với múi giờ theo định dạng ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Ví dụ đầu ra:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Sâu hơn
Lịch sử, việc chuyển đổi ngày thành chuỗi đã trở thành một phần không thể thiếu trong lập trình do nhu cầu biểu diễn ngày dưới dạng dễ đọc.

Các phương thức thay thế cho `strftime` bao gồm sử dụng phương thức `isoformat` cho định dạng ISO 8601, hoặc các thư viện bên thứ ba như `arrow` và `dateutil` cung cấp các tùy chọn phân tích cú pháp và định dạng linh hoạt hơn.

Về mặt triển khai, `strftime` có nghĩa là "định dạng thời gian chuỗi" và có nguồn gốc từ lập trình C. `strftime` của Python diễn giải các mã định dạng như `%Y` cho năm và `%m` cho tháng, cho phép tùy chỉnh gần như không giới hạn.

## Xem thêm
Để tìm hiểu sâu hơn về các chức năng ngày và thời gian của Python:
- Tài liệu chính thức `datetime` của Python: https://docs.python.org/3/library/datetime.html
- Đối với những ai quan tâm đến danh sách đầy đủ các chỉ thị `strftime`: https://strftime.org/
- Để khám phá các thư viện thời gian/ngày tháng bên thứ ba:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/

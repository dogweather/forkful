---
aliases:
- /vi/python/converting-a-date-into-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:25.862239-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE1ng th\xE0nh x\xE2u k\xFD t\u1EF1\
  \ bi\u1EBFn \u0111\u1ED5i m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\xE1\
  ng th\xE0nh \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB ng\xE0\
  y th\xE1ng\u2026"
lastmod: 2024-02-18 23:08:50.280636
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE1ng th\xE0nh x\xE2u k\xFD t\u1EF1\
  \ bi\u1EBFn \u0111\u1ED5i m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\xE1\
  ng th\xE0nh \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB ng\xE0\
  y th\xE1ng\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chuyển đổi ngày tháng thành xâu ký tự biến đổi một đối tượng ngày tháng thành định dạng văn bản. Lập trình viên thực hiện điều này để hiển thị ngày tháng một cách thân thiện với người dùng hoặc để chuẩn bị chúng cho việc lưu trữ trong các định dạng dựa trên văn bản như CSV hoặc JSON.

## Làm thế nào:
Python làm cho việc chuyển đổi ngày tháng thành xâu ký tự trở nên dễ dàng. Sử dụng phương thức `strftime` có sẵn trên các đối tượng ngày tháng. Dưới đây là cách làm:

```Python
from datetime import datetime

# Lấy ngày và giờ hiện tại
now = datetime.now()

# Chuyển thành xâu ký tự theo định dạng: Tháng ngày, Năm
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Kết quả: March 29, 2023 (hoặc ngày hiện tại)

# Định dạng: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Kết quả: 2023-03-29 (hoặc ngày hiện tại)
```

## Sâu hơn
Trong lịch sử, việc chuyển đổi ngày tháng thành xâu ký tự đã trở thành một phần cốt lõi trong lập trình do nhu cầu biểu diễn ngày tháng theo định dạng dễ đọc cho con người.

Các phương án thay thế cho `strftime` bao gồm sử dụng phương thức `isoformat` cho định dạng ISO 8601, hoặc các thư viện bên thứ ba như `arrow` và `dateutil` cung cấp nhiều tùy chọn phân tích và định dạng linh hoạt hơn.

Về mặt triển khai, `strftime` có nghĩa là "định dạng xâu thời gian" và có nguồn gốc từ lập trình C. `Strftime` của Python diễn giải các mã định dạng như `%Y` cho năm và `%m` cho tháng, cho phép tùy chỉnh gần như không giới hạn.

## Xem thêm
Để đào sâu hơn vào các chức năng ngày và giờ của Python:
- Tài liệu chính thức về `datetime` của Python: https://docs.python.org/3/library/datetime.html
- Dành cho những ai quan tâm đến danh sách toàn diện các hướng dẫn `strftime`: https://strftime.org/
- Để khám phá các thư viện thứ ba về ngày/giờ:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/

---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases:
- /vi/python/parsing-a-date-from-a-string/
date:                  2024-01-28T22:04:37.315842-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Phân tích cú pháp ngày từ một chuỗi nghĩa là chuyển đổi văn bản thành một đối tượng ngày tháng. Chúng ta làm điều này vì việc thao tác, tính toán sự khác biệt, hoặc định dạng ngày tháng sẽ dễ dàng hơn khi chúng không còn bị giữ dưới dạng văn bản thuần.

## Làm thế nào:
Mô-đun `datetime` của Python là sự lựa chọn hàng đầu của bạn để phân tích cú pháp ngày. Dưới đây là hướng dẫn nhanh:

```python
from datetime import datetime

date_string = "2023-04-01"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)  # Đầu ra: 2023-04-01 00:00:00

# Muốn xem một định dạng khác? Hãy thử "ngày-tháng-năm".
another_date_string = "01-April-2023"
another_date_object = datetime.strptime(another_date_string, "%d-%B-%Y")

print(another_date_object)  # Đầu ra: 2023-04-01 00:00:00
```

## Tìm hiểu sâu hơn
Phân tích cú pháp đã trở nên thiết yếu kể từ khi cơ sở dữ liệu và giao diện người dùng bắt đầu "nhảy múa" cùng nhau. Trước đây, dữ liệu thường được lưu trữ dưới dạng chuỗi, kể cả ngày tháng. Tuy nhiên, giờ đây, chúng ta có mô-đun `datetime` được giới thiệu trong Python 2.3 (và đã được cải thiện đáng kể kể từ đó).

Bạn không chỉ gói gọn trong `datetime`. Bạn có thể sử dụng các thư viện bên thứ ba như `dateutil`, được xem là linh hoạt hơn với các định dạng, hoặc `pandas` cho công việc phân tích dữ liệu nặng nhọc.

Về mặt triển khai, `strptime` có nghĩa là "string parse time" và sử dụng các mã định dạng để nhận diện mẫu. Điều này có nghĩa là bạn phải thông báo cho Python biết định dạng của chuỗi ngày, như `%Y` cho năm bốn chữ số hoặc `%d` cho ngày.

## Xem thêm
- Tài liệu datetime: https://docs.python.org/3/library/datetime.html
- Phân tích cú pháp của Dateutil: https://dateutil.readthedocs.io/en/stable/parser.html
- Hàm to_datetime của Pandas: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html

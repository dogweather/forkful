---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:37.315842-07:00
description: "L\xE0m th\u1EBF n\xE0o: M\xF4-\u0111un `datetime` c\u1EE7a Python l\xE0\
  \ s\u1EF1 l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7u c\u1EE7a b\u1EA1n \u0111\u1EC3\
  \ ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y. D\u01B0\u1EDBi \u0111\xE2y l\xE0 h\u01B0\
  \u1EDBng d\u1EABn nhanh."
lastmod: '2024-03-13T22:44:36.109910-06:00'
model: gpt-4-0125-preview
summary: "M\xF4-\u0111un `datetime` c\u1EE7a Python l\xE0 s\u1EF1 l\u1EF1a ch\u1ECD\
  n h\xE0ng \u0111\u1EA7u c\u1EE7a b\u1EA1n \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1\
  p ng\xE0y."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

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

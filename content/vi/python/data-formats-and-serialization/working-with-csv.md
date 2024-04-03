---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:27.414695-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi c\xE1c t\u1EC7p CSV (Comma-Separated Values\
  \ - Gi\xE1 tr\u1ECB Ph\xE2n c\xE1ch b\u1EDFi D\u1EA5u ph\u1EA9y) c\xF3 ngh\u0129\
  a l\xE0 \u0111\u1ECDc t\u1EEB v\xE0 ghi d\u1EEF li\u1EC7u v\xE0o c\xE1c t\u1EC7\
  p v\u0103n b\u1EA3n \u0111\u01A1n gi\u1EA3n, n\u01A1i m\u1ED7i\u2026"
lastmod: '2024-03-13T22:44:36.126641-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi c\xE1c t\u1EC7p CSV (Comma-Separated Values -\
  \ Gi\xE1 tr\u1ECB Ph\xE2n c\xE1ch b\u1EDFi D\u1EA5u ph\u1EA9y) c\xF3 ngh\u0129a\
  \ l\xE0 \u0111\u1ECDc t\u1EEB v\xE0 ghi d\u1EEF li\u1EC7u v\xE0o c\xE1c t\u1EC7\
  p v\u0103n b\u1EA3n \u0111\u01A1n gi\u1EA3n, n\u01A1i m\u1ED7i h\xE0ng l\xE0 m\u1ED9\
  t b\u1EA3n ghi d\u1EEF li\u1EC7u."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Cách thực hiện:
```python
# Nhập mô-đun CSV
import csv

# Đọc một tệp CSV
with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Kết quả đầu ra:
# ['Name', 'Age', 'City']
# ['Alice', '30', 'New York']
# ...

# Ghi vào một tệp CSV
with open('output.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Name', 'Age', 'City'])
    writer.writerow(['Bob', '22', 'Los Angeles'])

# Kiểm tra output.csv để xem kết quả
```

## Đi sâu hơn
Vào thời điểm truyền dữ liệu chậm và chi phí lưu trữ cao, CSV nhận được sự ủng hộ vì sự đơn giản và gánh nặng thấp của nó. Các lựa chọn khác như JSON và XML cung cấp cấu trúc nhưng phải trả giá bằng sự dài dòng. Đối với CSV, tốc độ phân tích là một ưu điểm, nhưng nó có thể gặp khó khăn với các cấu trúc phức tạp hoặc các loại dữ liệu.

Các thư viện như `pandas` cũng có thể xử lý CSV, cung cấp nhiều sức mạnh hơn nhưng đòi hỏi nhiều tài nguyên hơn. Bên dưới bề mặt, csv.reader() là một generator, tạo ra các hàng một lần một - thông minh cho quản lý bộ nhớ.

## Xem thêm
- Tài liệu đọc/viết CSV của Python: https://docs.python.org/3/library/csv.html
- Thư viện `pandas` để xử lý dữ liệu phức tạp: https://pandas.pydata.org/
- CSV vs. JSON vs. XML: Một so sánh giữa các định dạng dữ liệu: https://www.datacamp.com/community/tutorials/json-xml-csv

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:27.414695-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: V\xE0o th\u1EDDi \u0111i\u1EC3m truy\u1EC1\
  n d\u1EEF li\u1EC7u ch\u1EADm v\xE0 chi ph\xED l\u01B0u tr\u1EEF cao, CSV nh\u1EAD\
  n \u0111\u01B0\u1EE3c s\u1EF1 \u1EE7ng h\u1ED9 v\xEC s\u1EF1 \u0111\u01A1n gi\u1EA3\
  n v\xE0 g\xE1nh n\u1EB7ng th\u1EA5p c\u1EE7a n\xF3. C\xE1c l\u1EF1a ch\u1ECDn\u2026"
lastmod: '2024-04-05T21:53:37.553573-06:00'
model: gpt-4-0125-preview
summary: "V\xE0o th\u1EDDi \u0111i\u1EC3m truy\u1EC1n d\u1EEF li\u1EC7u ch\u1EADm\
  \ v\xE0 chi ph\xED l\u01B0u tr\u1EEF cao, CSV nh\u1EADn \u0111\u01B0\u1EE3c s\u1EF1\
  \ \u1EE7ng h\u1ED9 v\xEC s\u1EF1 \u0111\u01A1n gi\u1EA3n v\xE0 g\xE1nh n\u1EB7ng\
  \ th\u1EA5p c\u1EE7a n\xF3."
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

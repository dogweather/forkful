---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:37.058369-07:00
description: "L\xE0m th\u1EBF n\xE0o: CSV \u0111\xE3 t\u1ED3n t\u1EA1i t\u1EEB nh\u1EEF\
  ng ng\xE0y \u0111\u1EA7u c\u1EE7a ng\xE0nh tin h\u1ECDc, mang l\u1EA1i m\u1ED9t\
  \ c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 di chuy\u1EC3n d\u1EEF li\u1EC7u\
  \ b\u1EA3ng gi\u1EEFa c\xE1c ch\u01B0\u01A1ng tr\xECnh v\xE0 h\u1EC7 th\u1ED1ng.\u2026"
lastmod: '2024-04-05T22:50:51.645427-06:00'
model: gpt-4-0125-preview
summary: "CSV \u0111\xE3 t\u1ED3n t\u1EA1i t\u1EEB nh\u1EEFng ng\xE0y \u0111\u1EA7\
  u c\u1EE7a ng\xE0nh tin h\u1ECDc, mang l\u1EA1i m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 di chuy\u1EC3n d\u1EEF li\u1EC7u b\u1EA3ng gi\u1EEFa c\xE1c ch\u01B0\
  \u01A1ng tr\xECnh v\xE0 h\u1EC7 th\u1ED1ng."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Làm thế nào:
**Đọc một tệp CSV:**

```Ruby
require 'csv'

CSV.foreach("path/to/file.csv", headers: true) do |row|
  puts row["HeaderName"] # Thay thế bằng tên đầu tiêu thực tế của bạn
end
```

**Viết vào một tệp CSV:**

```Ruby
require 'csv'

CSV.open("path/to/output.csv", "wb", write_headers: true, headers: ["Tên", "Tuổi", "Thành phố"]) do |csv|
  csv << ["Alice", 32, "Wonderland"]
  csv << ["Bob", 46, "Springfield"]
end
```

**Kết quả mẫu:**

```Text
Alice, 32, Wonderland
Bob, 46, Springfield
```

## Tìm hiểu sâu
CSV đã tồn tại từ những ngày đầu của ngành tin học, mang lại một cách đơn giản để di chuyển dữ liệu bảng giữa các chương trình và hệ thống. Các phương án thay thế bao gồm JSON và XML, nhưng CSV vẫn phổ biến vì tính đơn giản và chi phí thấp. Thư viện CSV chuẩn của Ruby, được bao bọc tiện lợi quanh các trình phân tích cú pháp cơ bản, cung cấp sự tích hợp mượt mà bao gồm hỗ trợ cho các mã hóa khác nhau, chuyển đổi tùy chỉnh và các tùy chọn phân tích cú pháp linh hoạt.

## Xem thêm
- Tài liệu thư viện CSV của Ruby: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- CSV trên Wikipedia: https://en.wikipedia.org/wiki/Comma-separated_values
- "FasterCSV" gem (cũ nhưng có ý nghĩa về mặt lịch sử): https://rubygems.org/gems/fastercsv

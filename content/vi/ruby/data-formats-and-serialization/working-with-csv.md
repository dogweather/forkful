---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:37.058369-07:00
description: "T\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB ph\xE2n c\xE1\
  ch b\u1EB1ng d\u1EA5u ph\u1EA9y) bi\u1EC3u di\u1EC5n d\u1EEF li\u1EC7u b\u1EA3ng\
  \ d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n t\xFAy. C\xE1c l\u1EADp\
  \ tr\xECnh vi\xEAn s\u1EED d\u1EE5ng CSV \u0111\u1EC3\u2026"
lastmod: '2024-03-11T00:14:10.673480-06:00'
model: gpt-4-0125-preview
summary: "T\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB ph\xE2n c\xE1ch b\u1EB1\
  ng d\u1EA5u ph\u1EA9y) bi\u1EC3u di\u1EC5n d\u1EEF li\u1EC7u b\u1EA3ng d\u01B0\u1EDB\
  i d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n t\xFAy. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng CSV \u0111\u1EC3\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tệp CSV (Comma-Separated Values - Giá trị phân cách bằng dấu phẩy) biểu diễn dữ liệu bảng dưới dạng văn bản thuần túy. Các lập trình viên sử dụng CSV để nhập và xuất bộ dữ liệu bởi vì chúng được hỗ trợ rộng rãi, dễ đọc và dễ dàng phân tích cú pháp.

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

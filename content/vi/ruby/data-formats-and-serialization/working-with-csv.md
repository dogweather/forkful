---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:37.058369-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.056214-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 x\u1EED\
  \ l\xFD d\u1EEF li\u1EC7u d\u1EA1ng b\u1EA3ng \u0111\u01B0\u1EE3c l\u01B0u tr\u1EEF\
  \ d\u01B0\u1EDBi \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n\u2026"
lastmod: '2024-03-11T00:14:09.429895-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 x\u1EED\
  \ l\xFD d\u1EEF li\u1EC7u d\u1EA1ng b\u1EA3ng \u0111\u01B0\u1EE3c l\u01B0u tr\u1EEF\
  \ d\u01B0\u1EDBi \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n thu\u1EA7n\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Làm việc với CSV trong Clojure: Cái gì và Tại sao?

Làm việc với CSV (Comma-Separated Values - Giá trị tách biệt bằng dấu phẩy) có nghĩa là xử lý dữ liệu dạng bảng được lưu trữ dưới định dạng văn bản thuần túy. Các lập trình viên thực hiện điều này bởi việc xử lý CSV là nhu cầu phổ biến cho việc trao đổi dữ liệu và lưu trữ nhanh chóng, vì nó dễ đọc, đơn giản và được hỗ trợ bởi nhiều công cụ.

## Cách thực hiện:

Hãy gấp tay áo lên và phân tích một tập tin CSV trong Clojure.

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "data.csv")]
  (let [data (csv/read-csv reader)]
    (doseq [row data]
      (println row))))
```

Đầu ra mẫu cho một CSV với "name, age" có thể là:

```Clojure
["John" "30"]
["Jane" "25"]
["Doe" "40"]
```

Để ghi dữ liệu vào một tập tin CSV:

```Clojure
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["name" "age"]
                         ["John" "30"]
                         ["Jane" "25"]
                         ["Doe" "40"]]))
```

Điều này sẽ ghi các hàng đã cho vào `output.csv`.

## Sâu hơn

Xử lý CSV trong Clojure khá là đơn giản so với các ngôn ngữ khác - không thêm rườm rà. Lịch sử, sự đơn giản của CSV khiến nó phổ biến rộng rãi cho trao đổi dữ liệu, trước nhiều định dạng dữ liệu khác. Các lựa chọn thay thế bao gồm JSON, XML, hoặc YAML, nhưng CSV chiến thắng ở nơi đơn giản hoặc tương thích với bảng tính là quan trọng. Thư viện `clojure.data.csv` cung cấp những công cụ cần thiết cho việc phân tích và viết CSV, xây dựng trên dòng I/O hiệu quả của Java cho hiệu suất tốt.

## Xem thêm

1. Thư viện CSV của Clojure: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
2. Tìm hiểu thêm về CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
3. Để tìm hiểu sâu hơn về Clojure: [https://clojure.org/](https://clojure.org/)

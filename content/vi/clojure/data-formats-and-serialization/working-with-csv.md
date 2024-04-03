---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.056214-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y g\u1EA5p tay \xE1o l\xEAn v\xE0\
  \ ph\xE2n t\xEDch m\u1ED9t t\u1EADp tin CSV trong Clojure."
lastmod: '2024-03-13T22:44:36.182404-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y g\u1EA5p tay \xE1o l\xEAn v\xE0 ph\xE2n t\xEDch m\u1ED9t t\u1EAD\
  p tin CSV trong Clojure."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

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

---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:03:59.620711-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Đọc HTML là hành động biến một chuỗi HTML thành một cấu trúc dữ liệu mà chương trình của bạn có thể hiểu và thao tác. Lập trình viên làm việc này để tương tác với, trích xuất và sửa đổi nội dung từ mạng.

## Làm thế nào:

Để đọc HTML trong Clojure, chúng tôi sử dụng thư viện `clj-tagsoup`, một bộ bọc cho thư viện Tagsoup Java rất tiện lợi để đọc HTML thực tế.

Đầu tiên, thêm phụ thuộc clj-tagsoup vào dự án của bạn:

```clojure
[clj-tagsoup "0.3.3"] ; Kiểm tra phiên bản mới nhất
```

Bây giờ, hãy đọc một số HTML:

```clojure
(require '[clj-tagsoup.core :as tagsoup])

; Đọc HTML và nhận một vector của các bản đồ đại diện cho các phần tử được đọc
(def parsed-html (tagsoup/parse-string "<html><body><p>Hello, World!</p></body></html>"))

; Truy cập các phần tử
(println (first parsed-html))
```

Kết quả mẫu:

```clojure
{:tag :html, :attrs {}, :content [...]}
```

Để trích xuất các phần tử cụ thể, như đoạn văn:

```clojure
(defn extract-paragraphs [html]
  (let [parsed (tagsoup/parse-string html)]
    (filter #(= :p (:tag %)) parsed)))

; Sử dụng
(extract-paragraphs "<p>Đầu tiên</p><p>Thứ hai</p>")
```

## Sâu hơn nữa

Đọc HTML trong Clojure, giống như với các ngôn ngữ khác, thường liên quan đến việc duyệt qua một cấu trúc giống như cây. Ngày xưa, điều này có thể gây rối. Các thư viện như Tagsoup làm cho cuộc sống dễ dàng hơn bằng cách xử lý HTML thực tế kỳ quặc.

Bản chất hàm của Clojure cho phép chúng ta thao tác dữ liệu HTML một cách mượt mà. Các thư viện như `clj-tagsoup` tận dụng các công cụ đã được kiểm định của Java trong khi thêm vào sự tinh tế của Clojure.

Các thư viện thay thế bao gồm `Enlive` và `Hickory`. Enlive chuyên về cả đọc và tạo mẫu, cho phép thực hiện các thao tác phức tạp hơn. Hickory chuyển HTML thành các cấu trúc dữ liệu Clojure cho những người thích một giải pháp Clojure thuần túy.

Việc triển khai tập trung vào sự dễ dàng và phong cách khai báo. Bên dưới lớp vỏ, `clj-tagsoup` sử dụng các bộ định vị và điều hướng viên để duyệt qua HTML, cung cấp một trừu tượng cao hơn so với thao tác DOM trực tiếp.

## Xem thêm

- clj-tagsoup trên GitHub: https://github.com/nathell/clj-tagsoup
- Tagsoup, thư viện Java nằm dưới: https://github.com/McCLIM/cl-tagsoup
- Enlive, một thư viện phân tích HTML Clojure khác: https://github.com/cgrand/enlive
- Hickory, một dự án Clojure để đọc HTML: https://github.com/davidsantiago/hickory

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:59.620711-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 \u0111\u1ECDc HTML trong Clojure,\
  \ ch\xFAng t\xF4i s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `clj-tagsoup`, m\u1ED9t b\u1ED9\
  \ b\u1ECDc cho th\u01B0 vi\u1EC7n Tagsoup Java r\u1EA5t ti\u1EC7n l\u1EE3i \u0111\
  \u1EC3 \u0111\u1ECDc HTML th\u1EF1c t\u1EBF.\u2026"
lastmod: '2024-03-13T22:44:36.150135-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 \u0111\u1ECDc HTML trong Clojure, ch\xFAng t\xF4i s\u1EED d\u1EE5\
  ng th\u01B0 vi\u1EC7n `clj-tagsoup`, m\u1ED9t b\u1ED9 b\u1ECDc cho th\u01B0 vi\u1EC7\
  n Tagsoup Java r\u1EA5t ti\u1EC7n l\u1EE3i \u0111\u1EC3 \u0111\u1ECDc HTML th\u1EF1\
  c t\u1EBF."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

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

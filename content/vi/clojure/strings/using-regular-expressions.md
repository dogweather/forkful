---
aliases:
- /vi/clojure/using-regular-expressions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:19.727688-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) t\xECm ki\u1EBFm, kh\u1EDB\
  p v\xE0 thao t\xE1c chu\u1ED7i. Ch\xFAng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng v\xEC\
  \ t\xEDnh linh ho\u1EA1t v\xE0 hi\u1EC7u qu\u1EA3 trong nhi\u1EC7m v\u1EE5 x\u1EED\
  \ l\xFD v\u0103n b\u1EA3n."
lastmod: 2024-02-18 23:08:50.303635
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) t\xECm ki\u1EBFm, kh\u1EDBp v\xE0\
  \ thao t\xE1c chu\u1ED7i. Ch\xFAng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng v\xEC t\xED\
  nh linh ho\u1EA1t v\xE0 hi\u1EC7u qu\u1EA3 trong nhi\u1EC7m v\u1EE5 x\u1EED l\xFD\
  \ v\u0103n b\u1EA3n."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) tìm kiếm, khớp và thao tác chuỗi. Chúng được sử dụng vì tính linh hoạt và hiệu quả trong nhiệm vụ xử lý văn bản.

## Cách thực hiện:

```clojure
(require '[clojure.string :as str])

;; 1. Khớp
(re-matches #"\d+" "123")               ;; => "123"
(re-matches #"\d+" "abc")               ;; => nil

;; 2. Tìm kiếm
(re-find #"\d+" "Order 100 apples")     ;; => "100"

;; 3. Thay thế
(str/replace "2023-03-15" #"\d{4}" "YYYY") ;; => "YYYY-03-15"

;; 4. Chia
(str/split "one,two,three" #",")       ;; => ["one" "two" "three"]
```

## Sâu hơn
Biểu thức chính quy có một lịch sử phong phú, trở lại với công trình lý thuyết vào những năm 1950 của Stephen Cole Kleene. Các phương pháp thay thế cho regex bao gồm các hàm chuỗi như `indexOf`, `substring` và các thư viện phân tích cú pháp; tuy nhiên, regex thường cung cấp một giải pháp ngắn gọn hơn. Khả năng regex của Clojure được xây dựng trên lớp `Pattern` của Java, cung cấp khả năng khớp mẫu mạnh mẽ ngay trong ngôn ngữ.

## Xem thêm
- [ClojureDocs về Biểu Thức Chính Quy](https://clojuredocs.org/clojure.core/re-find)
- [Lớp Pattern của Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)

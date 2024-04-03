---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:19.727688-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.139544-06:00'
model: gpt-4-0125-preview
summary: .
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

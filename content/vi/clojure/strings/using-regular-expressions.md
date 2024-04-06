---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:19.727688-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Bi\u1EC3u th\u1EE9c ch\xEDnh quy c\xF3\
  \ m\u1ED9t l\u1ECBch s\u1EED phong ph\xFA, tr\u1EDF l\u1EA1i v\u1EDBi c\xF4ng tr\xEC\
  nh l\xFD thuy\u1EBFt v\xE0o nh\u1EEFng n\u0103m 1950 c\u1EE7a Stephen Cole Kleene.\
  \ C\xE1c ph\u01B0\u01A1ng ph\xE1p\u2026"
lastmod: '2024-04-05T21:53:37.565685-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy c\xF3 m\u1ED9t l\u1ECBch s\u1EED phong\
  \ ph\xFA, tr\u1EDF l\u1EA1i v\u1EDBi c\xF4ng tr\xECnh l\xFD thuy\u1EBFt v\xE0o nh\u1EEF\
  ng n\u0103m 1950 c\u1EE7a Stephen Cole Kleene."
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

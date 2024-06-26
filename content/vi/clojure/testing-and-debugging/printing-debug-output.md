---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.935328-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong Clojure, b\u1EA1n th\u01B0\u1EDDng xuy\xEA\
  n in ra \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i s\u1EED d\u1EE5ng `println`,\
  \ `printf`, `pr`, ho\u1EB7c `prn`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1\
  n r\u1EA3i r\xE1c m\u1ED9t s\u1ED1 in\u2026"
lastmod: '2024-03-13T22:44:36.156675-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, b\u1EA1n th\u01B0\u1EDDng xuy\xEAn in ra \u0111\u1EA7u ra\
  \ \u0111\u1EC3 g\u1EE1 l\u1ED7i s\u1EED d\u1EE5ng `println`, `printf`, `pr`, ho\u1EB7\
  c `prn`."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm Thế Nào:
Trong Clojure, bạn thường xuyên in ra đầu ra để gỡ lỗi sử dụng `println`, `printf`, `pr`, hoặc `prn`. Dưới đây là cách bạn rải rác một số in để gỡ lỗi:

```Clojure
(defn add-and-print [a b]
  (println "Đang cộng:" a "và" b) ; In ra phép toán
  (let [result (+ a b)]
    (println "Kết quả:" result)  ; In ra kết quả
    result))                    ; Trả về kết quả

(add-and-print 3 4)
```
Đầu Ra Mẫu:
```
Đang cộng: 3 và 4
Kết quả: 7
```

Hoặc, để gỡ lỗi các giá trị ở giữa của một macro luồng:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; In ra kết quả trung gian
    (* 2))
```
Đầu Ra Mẫu:
```
8
```

## Sâu Hơn:
In gỡ lỗi có một lịch sử lâu dài, có lẽ cũng cổ xưa như chính việc lập trình. Nó rất đơn giản: bạn chèn các câu lệnh in ở nơi bạn nghi ngờ có thể có vấn đề, chạy mã, và xem đầu ra.

Các hàm của Clojure để in gỡ lỗi khá giống với những hàm trong các ngôn ngữ Lisp khác, nhưng với phong cách hàm hóa quen thuộc. `println` và `prn` khác nhau ở chỗ `prn` viết dữ liệu theo cách có thể được đọc bởi trình đọc Clojure. `pprint` (in đẹp) từ `clojure.pprint` có thể được sử dụng khi bạn muốn định dạng đẹp hơn.

Một công cụ chuyên biệt của Clojure cho gỡ lỗi là `tap>`. Được giới thiệu trong Clojure 1.10, nó cho phép 'chạm nhẹ' vào mã đang chạy mà không cần phải lấp đầy mã của bạn với các câu lệnh in.

Đối với các dự án lớn hơn hoặc phức tạp hơn, cân nhắc sử dụng một thư viện lôg như `clojure.tools.logging` hoặc `timbre`.

## Xem Thêm:
- [`clojure.tools.logging`](https://github.com/clojure/tools.logging) kho lưu trữ GitHub
- [Thư viện lôg Timbre](https://github.com/ptaoussanis/timbre) kho lưu trữ GitHub
- [`clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) tài liệu trên ClojureDocs

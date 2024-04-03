---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:04.847123-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y th\u1EED nghi\u1EC7m v\u1EDBi JSON\
  \ trong Clojure. B\u1EA1n s\u1EBD c\u1EA7n `Cheshire`, m\u1ED9t th\u01B0 vi\u1EC7\
  n ph\u1ED5 bi\u1EBFn cho m\xE3 h\xF3a/gi\u1EA3i m\xE3 JSON. Tr\u01B0\u1EDBc ti\xEA\
  n, th\xEAm Cheshire v\xE0o\u2026"
lastmod: '2024-03-13T22:44:36.181163-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y th\u1EED nghi\u1EC7m v\u1EDBi JSON trong Clojure."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cách thực hiện:
Hãy thử nghiệm với JSON trong Clojure. Bạn sẽ cần `Cheshire`, một thư viện phổ biến cho mã hóa/giải mã JSON.

Trước tiên, thêm Cheshire vào phần phụ thuộc `project.clj` của bạn:
```clojure
[cheshire "5.10.1"]
```

Đọc JSON từ một chuỗi và chuyển đổi nó thành một bản đồ Clojure:
```clojure
(require '[cheshire.core :as json])

(def json-str "{\"name\":\"Clojure\"}")
(def clojure-map (json/parse-string json-str))

(println clojure-map)  ;; => {"name" "Clojure"}
```

Chuyển một bản đồ Clojure thành một chuỗi JSON:
```clojure
(def clojure-data {:language "Clojure" :cool true})
(def json-output (json/generate-string clojure-data))

(println json-output)  ;; => {"language":"Clojure","cool":true}
```

Phân tích JSON từ một tệp:
```clojure
(slurp "data.json")  ;; nội dung: {"message": "Chào, JSON!"}
(def file-content (slurp "data.json"))
(def message-data (json/parse-string file-content true))

(println message-data)  ;; => {"message" "Chào, JSON!"}
```

## Sâu hơn
Lịch sử của JSON bắt đầu với JavaScript, nhưng bây giờ nó được sử dụng rộng rãi, không phụ thuộc vào ngôn ngữ mẹ. Các lựa chọn khác? XML là lựa chọn phổ biến trước đây, tuy nhiên lại rườm rà hơn. YAML đơn giản, thân thiện với con người hơn nhưng không phổ biến như JSON trong các API. Về mặt triển khai: Clojure không phải là JavaScript, vì vậy các thư viện như Cheshire là cần thiết. Chúng kết nối bằng cách sử dụng các thư viện Java bên dưới để xử lý việc phân tích và tạo ra dữ liệu một cách hiệu quả.

## Xem thêm
- [Cheshire GitHub Repo](https://github.com/dakrone/cheshire): Để biết chi tiết và cập nhật thư viện.
- [JSON.org](https://www.json.org): Thông số kỹ thuật và chi tiết về JSON.
- [Clojure từ cơ bản đến nâng cao: JSON](https://aphyr.com/posts/305-clojure-from-the-ground-up-json): Hướng dẫn chi tiết về việc xử lý JSON với Clojure.

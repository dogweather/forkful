---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:04.847123-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 l\u01B0\
  u tr\u1EEF v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng JSON v\xEC s\u1EF1 d\u1EC5 s\u1EED d\u1EE5ng v\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.181163-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 l\u01B0\
  u tr\u1EEF v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng JSON v\xEC s\u1EF1 d\u1EC5 s\u1EED d\u1EE5ng v\u1EDBi\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cái gì & Tại sao?
JSON (JavaScript Object Notation) là một định dạng dữ liệu được sử dụng để lưu trữ và truyền tải dữ liệu. Lập trình viên sử dụng JSON vì sự dễ sử dụng với các web API và định dạng văn bản độc lập với ngôn ngữ, làm cho nó rất tiện lợi cho việc trao đổi dữ liệu.

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

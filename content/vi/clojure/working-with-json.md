---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:11:04.847123-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

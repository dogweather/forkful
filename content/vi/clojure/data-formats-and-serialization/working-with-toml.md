---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:58.030787-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi TOML\
  \ trong Clojure, b\u1EA1n c\u1EA7n m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `clj-toml`.\
  \ \u0110\u1EA7u ti\xEAn, th\xEAm n\xF3 v\xE0o `deps.edn` c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.183642-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi TOML trong Clojure, b\u1EA1n c\u1EA7\
  n m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `clj-toml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Để làm việc với TOML trong Clojure, bạn cần một thư viện như `clj-toml`. Đầu tiên, thêm nó vào `deps.edn` của bạn:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Sau đó phân tích một số TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Ví dụ TOML'")

(def parsed-config (toml/parse-string config-str))

;; Lấy tiêu đề từ TOML đã phân tích
(println (:title parsed-config)) ;; Xuất ra: Ví dụ TOML
```

Để tạo TOML:

```clojure
(def data {:title "Ví dụ TOML"})

(println (toml/generate-string data))
;; Xuất ra: title = "Ví dụ TOML"
```

## Sâu hơn
TOML được tạo ra vào khoảng năm 2013 bởi Tom Preston-Werner, đồng sáng lập GitHub, như một sự thay thế đơn giản hơn cho YAML và JSON cho các tập tin cấu hình. Nó hướng tới sự rõ ràng và có ý định là một thông số kỹ thuật mà con người có thể đọc mà không cần công cụ bổ sung.

Trong khi JSON thường được sử dụng cho API và ứng dụng web, và YAML có thể trở nên phức tạp với các tham chiếu và khả năng script, TOML nổi bật với trọng tâm vào cấu trúc đơn giản, dựa trên bảng. Sự đơn giản này làm cho nó đặc biệt phổ biến trong cộng đồng Rust và các môi trường ngôn ngữ hiện đại khác.

Clojure, với trọng tâm vào sự đơn giản và thiết thực, kết hợp tốt với TOML cho cấu hình. `clj-toml` hoặc các thư viện thay thế khác là cầu nối. Chúng chuyển đổi dữ liệu tĩnh của TOML thành thế giới động, chức năng của Clojure.

## Xem thêm
- Kho GitHub của TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` trên Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Tài liệu Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Giới thiệu `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)

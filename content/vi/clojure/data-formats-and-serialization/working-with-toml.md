---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:58.030787-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi TOML c\xF3 ngh\u0129a l\xE0 b\u1EA1n \u0111\
  ang x\u1EED l\xFD d\u1EEF li\u1EC7u theo \u0111\u1ECBnh d\u1EA1ng \"Ng\xF4n ng\u1EEF\
  \ T\u1ED1i thi\u1EC3u Hi\u1EC3n nhi\xEAn c\u1EE7a Tom\" (Tom's Obvious, Minimal\
  \ Language), ph\u1ED5 bi\u1EBFn\u2026"
lastmod: '2024-03-13T22:44:36.183642-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi TOML c\xF3 ngh\u0129a l\xE0 b\u1EA1n \u0111ang\
  \ x\u1EED l\xFD d\u1EEF li\u1EC7u theo \u0111\u1ECBnh d\u1EA1ng \"Ng\xF4n ng\u1EEF\
  \ T\u1ED1i thi\u1EC3u Hi\u1EC3n nhi\xEAn c\u1EE7a Tom\" (Tom's Obvious, Minimal\
  \ Language), ph\u1ED5 bi\u1EBFn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cái gì và Tại sao?
Làm việc với TOML có nghĩa là bạn đang xử lý dữ liệu theo định dạng "Ngôn ngữ Tối thiểu Hiển nhiên của Tom" (Tom's Obvious, Minimal Language), phổ biến cho các tập tin cấu hình do dễ đọc. Lập trình viên sử dụng nó cho việc quản lý cấu hình đơn giản, hoạt động ngay sau khi được cài đặt với cú pháp thân thiện với con người.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:48.272974-07:00
description: "L\xE0m th\u1EBF n\xE0o: Clojure kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3\
  \ t\xEDch h\u1EE3p s\u1EB5n cho YAML. B\u1EA1n s\u1EBD c\u1EA7n s\u1EED d\u1EE5\
  ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `clj-yaml`. \u0110\u1EA7u ti\xEAn, th\xEA\
  m n\xF3 v\xE0o ph\u1EA7n ph\u1EE5 thu\u1ED9c c\u1EE7a\u2026"
lastmod: '2024-03-13T22:44:36.179912-06:00'
model: gpt-4-0125-preview
summary: "Clojure kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3 t\xEDch h\u1EE3p s\u1EB5\
  n cho YAML."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm thế nào:
Clojure không bao gồm hỗ trợ tích hợp sẵn cho YAML. Bạn sẽ cần sử dụng một thư viện như `clj-yaml`. Đầu tiên, thêm nó vào phần phụ thuộc của bạn:

```clojure
;; Thêm vào project.clj hoặc deps.edn
[clj-yaml "0.7.0"]
```

Bây giờ, hãy phân tích một chuỗi YAML thành một bản đồ Clojure và ngược lại:

```clojure
(require '[clj-yaml.core :as yaml])

;; Phân tích chuỗi YAML thành bản đồ Clojure
(let [yaml-str "foo: bar\nbaz: 42"]
  (yaml/parse-string yaml-str))
;; => {"foo" "bar", "baz" 42}

;; Chuyển đổi bản đồ Clojure thành YAML
(let [clojure-map {"foo" "bar", "baz" 42}]
  (yaml/generate-string clojure-map))
;; Xuất ra chuỗi YAML:
;; foo: bar
;; baz: 42
```

## Sâu hơn
YAML được phát hành lần đầu vào năm 2001, với mục tiêu là dễ đọc hơn XML trong khi cung cấp cấu trúc dữ liệu phong phú hơn JSON. `clj-yaml` được xây dựng trên SnakeYAML, một thư viện Java, cho phép tương tác với các ngôn ngữ JVM. Các lựa chọn thay thế bao gồm việc sử dụng trực tiếp `org.yaml.snakeyaml` hoặc `cheshire` cho việc chuyển đổi JSON, vì JSON là một tập con của YAML.

## Xem thêm
Khám phá sâu hơn với những tài nguyên này:

- Trang chính thức của YAML: [https://yaml.org](https://yaml.org)
- Github cho clj-yaml: [https://github.com/clj-commons/clj-yaml](https://github.com/clj-commons/clj-yaml)
- SnakeYAML Engine: [https://bitbucket.org/asomov/snakeyaml-engine](https://bitbucket.org/asomov/snakeyaml-engine)

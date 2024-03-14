---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:48.272974-07:00
description: "YAML, \"YAML Ain't Markup Language,\" l\xE0 m\u1ED9t ti\xEAu chu\u1EA9\
  n h\xF3a d\u1EEF li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi d\xE0\
  nh cho t\u1EA5t c\u1EA3 ng\xF4n ng\u1EEF l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.179912-06:00'
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language,\" l\xE0 m\u1ED9t ti\xEAu chu\u1EA9n\
  \ h\xF3a d\u1EEF li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi d\xE0\
  nh cho t\u1EA5t c\u1EA3 ng\xF4n ng\u1EEF l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

YAML, "YAML Ain't Markup Language," là một tiêu chuẩn hóa dữ liệu thân thiện với con người dành cho tất cả ngôn ngữ lập trình. Các lập trình viên sử dụng YAML cho các tệp cấu hình và trao đổi dữ liệu nơi mà tính dễ đọc là quan trọng.

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

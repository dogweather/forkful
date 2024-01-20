---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 그리고 왜?)
YAML은 데이터를 표현하기 위한 간결한 형식입니다. 구성 파일, 데이터 교환 등에서 YAML을 사용해 데이터 관리의 단순화와 가독성을 높이기 위해 프로그래머들이 종종 사용합니다.

## How to: (어떻게 하나요?)
Clojure에서 YAML을 다루려면 'clj-yaml' 라이브러리를 사용합니다.

```Clojure
;; Leiningen/Boot을 사용해 clj-yaml dependency 추가
[clj-yaml "0.7.0"]

;; 예시 YAML 파일 파싱
(require '[clj-yaml.core :as yaml])

;; YAML 문자열을 Clojure 맵으로 로딩
(def yaml-str "age: 35\nname: John Doe")
(def parsed-yaml (yaml/parse-string yaml-str))
(println parsed-yaml) ; => {:age 35, :name "John Doe"}
```
## Deep Dive (깊이 탐구하기)
YAML은 "YAML Ain't Markup Language"의 줄임말로, 데이터를 사람이 읽고 쓸 수 있는 평문 형식으로 나타냅니다. 2001년에 개발되기 시작해 JSON 및 XML과 같은 형식과 대비됩니다. 이러한 대안들은 각각의 사용 사례와 기술 스택에 따라 다양한 장단점을 가집니다. Clojure에서는 `clj-yaml` 외에도 `snakeyaml` 라이브러리로 파싱하고, 데이터를 직렬화할 수 있습니다. 성능이나 메모리 사용량 같은 구현 세부 사항은 프로젝트의 요구 사항과 환경에 따라 중요할 수 있습니다.

## See Also (더 알아보기)
- YAML 공식 웹사이트: [https://yaml.org](https://yaml.org)
- YAML과 JSON 비교: [https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON](https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON)
- Clojure 공식 문서: [https://clojure.org](https://clojure.org)
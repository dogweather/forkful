---
title:                "yaml로 작업하기"
html_title:           "Clojure: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 데이터를 읽고 쓰기 위한 포맷으로 자주 사용되는 열린 표현 언어입니다. 프로그래머는 YAML을 사용하여 다양한 파일 형식을 읽고 쓰고, 설정 파일과 데이터 구조를 효율적으로 관리할 수 있습니다.

## 어떻게:

```Clojure
(require '[clojure.data.yaml :as yaml])

;; YAML 파일 읽기 예제
(def data (yaml/read-str "name: John\nage: 30"))
(println data)
;; 출력: {:name "John", :age 30}

;; YAML 파일 쓰기 예제
(def data {:name "Jane", :age 25})
(println (yaml/dump data))
;; 출력: "name: Jane\nage: 25"
```

## 깊이 파고들기:

YAML은 2001년에 처음 개발되었고, JSON이나 XML과 비슷한 형식을 가지고 있지만 읽기 쉽고 더 유연합니다. YAML은 다양한 프로그래밍 언어에서 지원되며, 설정 파일, 데이터 직렬화 등 다양한 용도로 사용됩니다. 대안으로는 JSON, XML, TOML 등이 있지만, YAML은 가독성과 유연성을 고려할 때 가장 좋은 선택입니다. YAML을 파싱하는 구현 디테일에 대해 알고 싶다면, [`clojure.data.yaml`](https://github.com/clj-commons/yaml) 라이브러리의 소스 코드를 살펴보세요.

## 참고 자료:

- [YAML 공식 사이트](https://yaml.org/)
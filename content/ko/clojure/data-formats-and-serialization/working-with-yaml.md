---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.577178-07:00
description: "\uC5B4\uB5BB\uAC8C: Clojure\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C YAML\uC5D0\
  \ \uB300\uD55C \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC9C0\
  \uB9CC, `clj-yaml`\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC YAML \uB370\uC774\uD130\uB97C \uD30C\
  \uC2F1\uD558\uACE0 \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800\
  , \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD504\uB85C\uC81D\uD2B8 \uC758\uC874\uC131\
  \uC5D0 \uCD94\uAC00\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.690978-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C YAML\uC5D0 \uB300\uD55C \uC9C0\
  \uC6D0\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, `clj-yaml`\uACFC\
  \ \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC YAML \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0 \uC0DD\
  \uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 어떻게:
Clojure는 기본적으로 YAML에 대한 지원을 포함하고 있지 않지만, `clj-yaml`과 같은 서드파티 라이브러리를 사용하여 YAML 데이터를 파싱하고 생성할 수 있습니다. 먼저, 라이브러리를 프로젝트 의존성에 추가하세요:

```clojure
;; 이것을 프로젝트.clj 의존성에 추가하세요
[clj-yaml "0.7.0"]
```

여기 `clj-yaml`을 사용하여 YAML을 파싱하고 Clojure 맵을 YAML로 변환하는 방법이 있습니다.

### YAML 파싱:
```clojure
(require '[clj-yaml.core :as yaml])

;; YAML 문자열 파싱하기
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; 출력:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Clojure에서 YAML 생성:
```clojure
(require '[clj-yaml.core :as yaml])

;; Clojure 맵을 YAML 문자열로 변환하기
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; 출력:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

이와 같은 간단한 작업들을 `clj-yaml`과 함께 Clojure 애플리케이션에 통합하여, 설정 파일을 다루거나 YAML을 사용하는 다른 서비스나 컴포넌트와의 데이터 교환을 용이하게 할 수 있습니다.

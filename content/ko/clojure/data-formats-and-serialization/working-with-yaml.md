---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.577178-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC7AC\uADC0\uC801\uC778\
  \ \uC57D\uC790\uB85C, \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\
  \uC774\uD130 \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC73C\uB85C\uC11C, \uC124\uC815 \uD30C\
  \uC77C\uACFC \uB2E4\uB978 \uB370\uC774\uD130 \uAD6C\uC870\uB97C \uAC00\uC9C4 \uC5B8\
  \uC5B4 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD574 \uC0AC\uC6A9\
  \uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 YAML\uC758 \uB2E8\
  \uC21C\uD568\uACFC \uAC00\uB3C5\uC131 \uB54C\uBB38\uC5D0 YAML\uC744\u2026"
lastmod: 2024-02-19 22:05:13.633295
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC7AC\uADC0\uC801\uC778\
  \ \uC57D\uC790\uB85C, \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\
  \uC774\uD130 \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC73C\uB85C\uC11C, \uC124\uC815 \uD30C\
  \uC77C\uACFC \uB2E4\uB978 \uB370\uC774\uD130 \uAD6C\uC870\uB97C \uAC00\uC9C4 \uC5B8\
  \uC5B4 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD574 \uC0AC\uC6A9\
  \uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 YAML\uC758 \uB2E8\
  \uC21C\uD568\uACFC \uAC00\uB3C5\uC131 \uB54C\uBB38\uC5D0 YAML\uC744\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 "YAML Ain't Markup Language"의 재귀적인 약자로, 사람이 읽을 수 있는 데이터 직렬화 포맷으로서, 설정 파일과 다른 데이터 구조를 가진 언어 간의 데이터 교환을 위해 사용됩니다. 프로그래머들은 YAML의 단순함과 가독성 때문에 YAML을 활용하는데, 이는 애플리케이션을 구성하고 다양한 프로그래밍 환경에서 데이터 교환을 용이하게 하는 데 이상적인 선택입니다.

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

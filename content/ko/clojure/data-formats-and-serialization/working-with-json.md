---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.226012-07:00
description: "\uBC29\uBC95: Clojure\uB294 JSON\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C\
  \ \uB0B4\uC7A5 \uD568\uC218\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC73C\
  \uBBC0\uB85C, \uC77C\uBC18\uC801\uC73C\uB85C \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. `cheshire`\uC640 `jsonista`\uB294 \uC0AC\
  \uC6A9\uD558\uAE30 \uC27D\uACE0 \uC131\uB2A5\uC774 \uC88B\uC544 \uC778\uAE30\uC788\
  \uB294 \uC120\uD0DD\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.529607-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 JSON\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uB0B4\uC7A5 \uD568\
  \uC218\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC77C\
  \uBC18\uC801\uC73C\uB85C \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 방법:
Clojure는 JSON을 다루기 위한 내장 함수를 포함하고 있지 않으므로, 일반적으로 제3자 라이브러리를 사용합니다. `cheshire`와 `jsonista`는 사용하기 쉽고 성능이 좋아 인기있는 선택입니다.

### Cheshire 사용하기
먼저, `project.clj`에 Cheshire를 프로젝트 의존성에 추가합니다:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

JSON 문자열을 Clojure 맵으로 파싱하고 맵을 JSON 문자열로 변환하는 방법:

```clj
(require '[cheshire.core :as json])

;; JSON 문자열을 Clojure 맵으로 파싱
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Clojure 맵을 JSON 문자열로 변환
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Jsonista 사용하기
Jsonista를 `project.clj`에 프로젝트에 추가하세요:
```clj
[jsonista "0.3.2"]
```

Jsonista로 비슷한 작업을 진행합니다:

```clj
(require '[jsonista.core :as j])

;; JSON 문자열을 Clojure로 파싱
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Clojure 맵을 JSON 문자열로 변환
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

두 라이브러리 모두 더 복잡한 데이터 구조를 인코딩 및 디코딩하는 옵션이 있으며, 직렬화 및 역직렬화 과정을 커스터마이징할 수 있는 추가적인 함수와 매개변수가 있습니다. 대부분의 애플리케이션에 대해서, 보여진 기능들은 Clojure 애플리케이션에서 JSON을 다루기 위한 탄탄한 기초를 제공합니다.

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.226012-07:00
description: "Clojure\uC5D0\uC11C JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8E8\
  \uB294 \uAC83\uC740 JSON \uBB38\uC790\uC5F4\uC744 Clojure \uB370\uC774\uD130 \uAD6C\
  \uC870\uCCB4(\uB9F5, \uBCA1\uD130)\uB85C \uD30C\uC2F1\uD558\uACE0 \uADF8 \uBC18\uB300\
  \uB85C \uBCC0\uD658\uD558\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uC774 \uC791\uC5C5\uC740 \uC6F9 \uC11C\uBE44\uC2A4, API, \uADF8\uB9AC\uACE0 \uAD6C\
  \uC870\uD654\uB41C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD3EC\uB9F7\uC73C\uB85C\u2026"
lastmod: '2024-03-11T00:14:28.607629-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC5D0\uC11C JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8E8\uB294\
  \ \uAC83\uC740 JSON \uBB38\uC790\uC5F4\uC744 Clojure \uB370\uC774\uD130 \uAD6C\uC870\
  \uCCB4(\uB9F5, \uBCA1\uD130)\uB85C \uD30C\uC2F1\uD558\uACE0 \uADF8 \uBC18\uB300\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\
  \ \uC791\uC5C5\uC740 \uC6F9 \uC11C\uBE44\uC2A4, API, \uADF8\uB9AC\uACE0 \uAD6C\uC870\
  \uD654\uB41C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD3EC\uB9F7\uC73C\uB85C\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Clojure에서 JSON(JavaScript Object Notation)을 다루는 것은 JSON 문자열을 Clojure 데이터 구조체(맵, 벡터)로 파싱하고 그 반대로 변환하는 작업을 포함합니다. 이 작업은 웹 서비스, API, 그리고 구조화된 텍스트 기반 포맷으로 데이터를 소통해야 하는 애플리케이션에 있어 기본적인 것이다. 왜냐하면 JSON은 다양한 프로그래밍 환경에 걸쳐 보편적으로 인식되고 지원되기 때문입니다.

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

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:57.430297-07:00
description: "\uBC29\uBC95: \uD074\uB85C\uC800\uB294 JVM \uC5B8\uC5B4\uC774\uAE30\
  \ \uB54C\uBB38\uC5D0 \uC790\uBC14\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uC9C1\uC811 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uB0B4\uC7A5\uB41C \uC790\uBC14 \uC0C1\uD638\uC6B4\uC6A9\uC131\uC73C\uB85C\
  \ \uC2DC\uC791\uD55C \uB2E4\uC74C, \uB354 \uAD00\uC6A9\uC801\uC778 \uD074\uB85C\uC800\
  \ \uC194\uB8E8\uC158\uC744 \uC704\uD574 \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uC778 clj-time\uC744 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\
  \uC744 \uD0D0\uAD6C\uD574\uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.515836-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uB294 JVM \uC5B8\uC5B4\uC774\uAE30 \uB54C\uBB38\uC5D0\
  \ \uC790\uBC14\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC9C1\uC811 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
클로저는 JVM 언어이기 때문에 자바의 날짜와 시간 라이브러리를 직접 사용할 수 있습니다. 내장된 자바 상호운용성으로 시작한 다음, 더 관용적인 클로저 솔루션을 위해 인기 있는 타사 라이브러리인 clj-time을 사용하는 방법을 탐구해보겠습니다.

### 자바 상호운용 사용하기
클로저는 자바의 `java.time.LocalDate`를 직접 활용하여 문자열에서 날짜를 파싱할 수 있습니다:
```clojure
(require '[clojure.java.io :as io])

; 자바 상호운용을 사용한 날짜 파싱
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; 출력: 2023-04-01
```

### clj-time 사용하기
날짜와 시간을 다루기 위한 더 관용적인 클로저 라이브러리는 `clj-time`입니다. 이것은 날짜와 시간 연산을 위한 포괄적인 라이브러리인 Joda-Time을 래핑합니다. 우선, 프로젝트의 의존성에 `clj-time`을 추가할 필요가 있습니다. 다음은 `clj-time`을 사용하여 날짜 문자열을 파싱하는 방법입니다:

```clojure
; 프로젝트.clj의 :dependencies 아래에 [clj-time "0.15.2"]를 추가하세요

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; 포맷터 정의하기
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; 출력: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

이 예시들은 기본적인 날짜 파싱을 보여줍니다. 두 방법 모두 유용하지만, `clj-time`은 복잡한 요구 사항을 위한 추가 기능과 함께 더 클로저 중심적인 접근 방식을 제공할 수 있습니다.

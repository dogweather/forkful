---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:51.314135-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Clojure\uC758 Java\uC640\uC758\
  \ \uB04A\uAE40\uC5C6\uB294 \uC0C1\uD638 \uC6B4\uC6A9\uC131\uC744 \uD1B5\uD574 Java\
  \ Date-Time API\uB97C \uC9C1\uC811 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC\
  \ \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.676527-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC758 Java\uC640\uC758 \uB04A\uAE40\uC5C6\uB294 \uC0C1\uD638 \uC6B4\
  \uC6A9\uC131\uC744 \uD1B5\uD574 Java Date-Time API\uB97C \uC9C1\uC811 \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 어떻게 하나요:


### Java 상호 운용성 사용하기
Clojure의 Java와의 끊김없는 상호 운용성을 통해 Java Date-Time API를 직접 사용할 수 있습니다. 현재 날짜를 얻는 방법은 다음과 같습니다:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; 샘플 출력
(get-current-date) ; "2023-04-15"
```

### clj-time 라이브러리 사용하기
더 Clojure다운 해결책을 원한다면, Joda-Time을 래핑하는 `clj-time` 라이브러리를 선택할 수 있습니다. 그러나 대부분의 새 프로젝트에서는 내장된 Java 8 Date-Time API가 권장됩니다. 그럼에도 불구하고 `clj-time`을 선호하거나 필요로 한다면:

먼저, 프로젝트 의존성에 `clj-time`을 추가합니다. `project.clj`에 다음을 포함시키세요:

```clojure
[clj-time "0.15.2"]
```

그 다음, 현재 날짜를 얻기 위해 사용하세요:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; 샘플 출력
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

두 방법 모두 Clojure에서 현재 날짜를 빠르고 효과적으로 얻는 방법을 제공하며, 기반 Java 플랫폼의 강력함이나 Clojure 특유의 편리함을 활용합니다.

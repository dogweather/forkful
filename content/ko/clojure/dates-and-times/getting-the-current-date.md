---
title:                "현재 날짜 가져오기"
aliases:
- /ko/clojure/getting-the-current-date.md
date:                  2024-02-03T19:09:51.314135-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래밍에서 현재 날짜를 얻는 것은 로깅, 이벤트 타임스탬핑, 작업 스케줄링 등 다양한 이유로 중요합니다. JVM상의 Lisp 방언인 Clojure에서, 이 작업은 Java 상호 운용성 기능을 활용하여 풍부한 Java Date-Time API에 대한 간편한 접근을 가능하게 합니다.

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

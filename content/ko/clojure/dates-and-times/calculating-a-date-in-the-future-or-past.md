---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- /ko/clojure/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:30:57.388306-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산은 미래나 과거의 특정 날짜를 정하는 것입니다. 예약 시스템, 기한 설정, 시간 관리 등 프로그래밍에서 자주 사용합니다.

## How to: (방법)
```Clojure
;; clojure.java.time 라이브러리를 사용합니다.

(require '[java-time :as jt])

;; 오늘 날짜를 구합니다.
(def today (jt/local-date))

;; 10일 후
(def ten-days-later (jt/plus-days today 10))

;; 10일 전
(def ten-days-before (jt/minus-days today 10))

(println "오늘: " today)
(println "10일 후: " ten-days-later)
(println "10일 전: " ten-days-before)
```
출력:
```
오늘: 2023-04-03
10일 후: 2023-04-13
10일 전: 2023-03-24
```

## Deep Dive (심층 분석)
날짜 계산을 위해 Clojure에서는 `clojure.java.time` 라이브러리를 주로 사용합니다. 이는 Joda-Time 라이브러리에 영감을 받은 자바 8의 java.time 패키지를 래핑한 것입니다. 이전에는 clj-time 라이브러리가 많이 쓰였지만, 현재는 `clojure.java.time`이 표준으로 자리잡고 있습니다. 

더 많은 날짜 관련 연산이 필요하다면 `.plusWeeks`, `.plusMonths` 같은 메서드를 사용할 수도 있습니다. 타임존을 다루려면 `jt/zoned-date-time` 인스턴스로 작업합니다. 내부적으로는 자바의 `java.time` 라이브러리와 같은 원리로 작동하니, 자바의 날짜와 시간 API에 대한 이해가 도움이 됩니다.

## See Also (관련 링크)
- Clojure 공식 문서: [https://clojure.org/](https://clojure.org/)
- `clojure.java.time` 라이브러리 문서: [https://clj-time.github.io/clj-time/doc/index.html](https://clj-time.github.io/clj-time/doc/index.html)
- 자바 8 `java.time` 레퍼런스: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)

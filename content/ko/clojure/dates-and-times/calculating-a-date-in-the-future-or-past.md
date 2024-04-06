---
date: 2024-01-20 17:30:57.388306-07:00
description: "How to: (\uBC29\uBC95) \uB0A0\uC9DC \uACC4\uC0B0\uC744 \uC704\uD574\
  \ Clojure\uC5D0\uC11C\uB294 `clojure.java.time` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC8FC\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\uB294 Joda-Time \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC5D0 \uC601\uAC10\uC744 \uBC1B\uC740 \uC790\uBC14 8\uC758 java.time\
  \ \uD328\uD0A4\uC9C0\uB97C \uB798\uD551\uD55C \uAC83\uC785\uB2C8\uB2E4. \uC774\uC804\
  \uC5D0\uB294 clj-time \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uB9CE\uC774\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.520065-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uB0A0\uC9DC \uACC4\uC0B0\uC744 \uC704\uD574 Clojure\uC5D0\
  \uC11C\uB294 `clojure.java.time` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC8FC\uB85C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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

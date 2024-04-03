---
date: 2024-01-20 17:33:00.488558-07:00
description: "\uB0A0\uC9DC\uB97C \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uB450 \uB0A0\
  \uC9DC\uAC00 \uC11C\uB85C \uC5B4\uB5A4 \uAD00\uACC4\uC5D0 \uC788\uB294\uC9C0, \uC989\
  \ \uB3D9\uC77C\uD55C\uC9C0, \uC774\uC804\uC774\uB098 \uC774\uD6C4\uC778\uC9C0\uB97C\
  \ \uC54C\uC544\uB0B4\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uB0A0\uC9DC \uBC94\uC704 \uD544\
  \uD130\uB9C1, \uC774\uBCA4\uD2B8 \uC2A4\uCF00\uC904\uB9C1\uACFC \uAC19\uC740 \uAE30\
  \uB2A5\uC744 \uC2E4\uD589\uD558\uAE30 \uC704\uD574 \uB0A0\uC9DC\uB97C \uBE44\uAD50\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.679379-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uB450 \uB0A0\uC9DC\
  \uAC00 \uC11C\uB85C \uC5B4\uB5A4 \uAD00\uACC4\uC5D0 \uC788\uB294\uC9C0, \uC989 \uB3D9\
  \uC77C\uD55C\uC9C0, \uC774\uC804\uC774\uB098 \uC774\uD6C4\uC778\uC9C0\uB97C \uC54C\
  \uC544\uB0B4\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (방법)
Clojure에서 날짜를 비교해보자. `clj-time` 라이브러리를 사용하여 예제를 살펴볼 수 있습니다. 

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

(let [date1 (t/now)
      date2 (t/plus (t/now) (t/days 1))]
  (println "Is date1 before date2?" (t/before? date1 date2))
  (println "Is date1 after date2?" (t/after? date1 date2))
  (println "Is date1 equal to date2?" (t/equal? date1 date2)))
```

샘플 출력:

```
Is date1 before date2? true
Is date1 after date2? false
Is date1 equal to date2? false
```

## Deep Dive (심층 분석)
날짜 비교 기능은 Clojure의 초기 버전부터 관심사였습니다. `clj-time` 라이브러리는 Joda-Time 라는 자바 라이브러리에 기초하여 만들어졌습니다. Joda-Time은 자바 8에 추가된 `java.time` 패키지의 영감이 되기도 했습니다. `clj-time`은 간편한 날짜 처리와 비교 기능을 제공하지만 최신 Clojure 버전에서는 자바의 `java.time`을 직접 사용하는 것도 좋습니다. 내부적으로, 날짜 비교는 밀리초 단위의 타임스탬프를 사용하여 두 날짜의 차이를 계산합니다. 각각의 함수(`before?`, `after?`, `equal?`)는 이 차이를 평가하여 boolean 값을 반환합니다.

## See Also (참조)
- clj-time GitHub 페이지: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- 자바의 `java.time` 패키지 문서: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Joda-Time 공식 웹 사이트: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)

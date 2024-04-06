---
date: 2024-01-20 17:33:00.488558-07:00
description: "How to: (\uBC29\uBC95) Clojure\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBE44\
  \uAD50\uD574\uBCF4\uC790. `clj-time` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uC608\uC81C\uB97C \uC0B4\uD3B4\uBCFC \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.519115-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Clojure\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD574\
  \uBCF4\uC790."
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

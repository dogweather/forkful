---
title:                "두 날짜 비교하기"
aliases: - /ko/clojure/comparing-two-dates.md
date:                  2024-01-20T17:33:00.488558-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

날짜를 비교하는 것은 두 날짜가 서로 어떤 관계에 있는지, 즉 동일한지, 이전이나 이후인지를 알아내는 과정입니다. 프로그래머들은 유효성 검사, 날짜 범위 필터링, 이벤트 스케줄링과 같은 기능을 실행하기 위해 날짜를 비교합니다.

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

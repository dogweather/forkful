---
title:    "Clojure: 두 개의 날짜 비교"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

나는 많은 프로그래머들이 비슷한 작업에 끊임없이 처음부터 코드를 다시 작성하는 것을 보았습니다. 이는 시간이 많이 소모되고 실수를 야기할 수 있기 때문에 매우 비효율적입니다. 그래서 오늘 우리는 두 개의 날짜를 비교하는 방법을 배우겠습니다.

## 왜?

이번 포스트에서 우리는 두 개의 날짜를 비교하고 그 차이를 알아내는 방법을 살펴보겠습니다. 이는 매우 일반적인 작업이며, 우리의 일상적인 프로그래밍에서 종종 발생할 수 있습니다. 또한, 이를 활용하면 시간 관련 데이터를 더 쉽게 처리할 수 있으며, 효율적인 코드 작성에 도움이 됩니다.

## 어떻게?

우리가 비교하고자 하는 두 날짜를 가져와 봅시다.

T-1:

```Clojure
(def d1 (local-date 2020 1 1))
(def d2 (local-date 2020 3 15))
```

우리는 이제 두 날짜를 비교하여 그 차이를 알아내기 위해 `-` 연산자를 사용할 수 있습니다.

T-2:

```Clojure
(def days (- d2 d1))
(def months (- (month d2) (month d1)))
(def years (- (year d2) (year d1)))

(println "두 날짜의 차이는" days "일," months "개월," years "년입니다.")
```

출력:

`두 날짜의 차이는 74 일, 2 개월, 0 년입니다.`

이처럼 우리는 매우 간단하게 두 날짜를 비교하고 그 차이를 알아냈습니다.

## 더 깊게 들어가기

이제 우리는 `between` 함수를 사용해보겠습니다. 이 함수는 두 날짜 사이의 모든 날짜를 리턴해줍니다. 예를 들어, 우리가 비교하고자 하는 날짜가 2020년 1월 1일부터 2020년 1월 7일까지라면, `between` 함수는 2020년 1월 1일, 2020년 1월 2일, ..., 2020년 1월 7일을 리스트 형태로 리턴해줍니다. 이를 다음과 같이 사용할 수 있습니다.

T-3:

```Clojure
(def d1 (local-date 2020 1 1))
(def d2 (local-date 2020 1 7))

(def days (between d1 d2))

(println "두 날짜 사이의 모든 날짜는 다음과 같습니다:")
(doseq [day days] (println day))
```

출력:

`두 날짜 사이의 모든 날짜는 다음과 같습니다:`

`2020-01-01`

`2020-01-02`

`2020-01-03`

`2020-01-04`

`2020-01-05`

`2020-01-06`

`2020-01-07`

## 다른 예제들 보기

- [ClojureDocs](https://clojuredocs.org/clojure.core/between)
- [Calculated Dates and Times in Clojure](http://codingfoster.blogspot.com/2012/03/calculated-dates-and-times-in-clojure.html)

---
## 같이 보기

- [Clojure로 간단하게 시작하기](https://clojure.or.kr/index.html)
- [ClojureDocs 한국어 번역 프로젝트](https://github.com/clojure-docs-kr
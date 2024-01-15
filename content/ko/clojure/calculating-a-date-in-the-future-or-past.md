---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Clojure: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 계산하는 것은 매우 유용한 기능입니다. 예를 들어, 특정 이벤트나 기념일까지 얼마나 남았는지 알고 싶을 때가 있고, 나중에 약속을 잡을 때 각자의 스케줄을 확인해야 할 때도 있습니다.

## 어떻게

```
Clojure (require 'clj-time.core)
(use 'clj-time.core)

(def today (org.joda.time.DateTime.)) ; 오늘 날짜 가져오기

(def future (plus today (days 30))) ; 30일 후의 날짜 계산하기

(def past (minus today (weeks 2))) ; 2주 전의 날짜 계산하기

(println "오늘 날짜: " today) ; 오늘 날짜 출력하기
(println "30일 후의 날짜: " future) ; 30일 후의 날짜 출력하기
(println "2주 전의 날짜: " past) ; 2주 전의 날짜 출력하기
```

위 코드를 실행하면 오늘 날짜를 기준으로 30일 후와 2주 전의 날짜를 계산하여 출력합니다.

## 더 들어가보기

이 예제에서는 날짜를 계산하는 기본적인 함수만 사용했지만, clj-time 라이브러리를 사용하면 더 복잡한 날짜 계산도 가능합니다. 또한, 오늘 날짜뿐만 아니라 원하는 날짜를 기준으로 계산도 가능합니다. 더 자세한 내용은 clj-time 라이브러리 문서를 참고하시기 바랍니다.

## 관련 링크

- [clj-time 라이브러리 문서](https://github.com/clj-time/clj-time)
- [Clojure 공식 홈페이지](https://clojure.org/)
- [Joda-Time 공식 홈페이지](https://joda.org/joda-time/)
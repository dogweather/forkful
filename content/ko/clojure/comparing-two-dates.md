---
title:                "Clojure: 두 날짜 비교하기"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하고자 하는 이유는 데이터 분석이나 예약 시스템 등 여러 가지 프로그래밍 작업에서 필요할 수 있기 때문입니다.

## 방법

Clojure에서 두 날짜를 비교하는 방법은 간단합니다. 먼저 `clojure.java-time` 라이브러리를 가져옵니다. 그 후에 두 개의 날짜를 `cljcod.Date` 객체로 변환합니다. 마지막으로 `clojure.java-time.before?` 함수를 사용하여 두 날짜를 비교할 수 있습니다.

```Clojure
(require '[java-time :as t])

;; 2020년 5월 10일과 2020년 5월 15일을 비교하는 예제입니다.
(def date1 (t/date 2020 5 10))
(def date2 (t/date 2020 5 15))

;; date1이 date2보다 이전인지 비교합니다.
(print (t/before? date1 date2))

;; 결과: true
```

## 깊게 파헤치기

날짜를 비교할 때에는 두 날짜의 시간대도 고려해야 합니다. `clojure.java-time.before?` 함수는 기본적으로 Unix Epoch과의 비교를 하기 때문에, 시간대가 다른 두 날짜를 비교할 때는 부정확한 결과가 나올 수 있습니다. 이를 방지하기 위해서는 `ZoneId` 객체를 사용하여 해당 날짜의 시간대를 지정해주어야 합니다.

`clojure.java-time` 라이브러리는 `ZoneId` 객체를 다루는 `t/time-zone` 함수를 제공합니다.

```Clojure
(def seoul-zone (t/time-zone "Asia/Seoul"))

;; 2020년 5월 10일과 2020년 5월 10일 12시를 비교하는 예제입니다.
(def date1 (t/date 2020 5 10))
(def date2 (t/local-date-time 2020 5 10 12))
(def date3 (t/zoned-date-time date2 seoul-zone))

;; date1이 date2보다 이전인지 비교합니다.
(print (t/before? date1 date2))

;; 결과: false

;; date1이 date3보다 이전인지 비교합니다.
(print (t/before? date1 date3))

;; 결과: true
```

## 더 알아보기

- `clojure.java-time` 라이브러리 공식 문서 (https://cljdoc.org/d/java-time/java-time/0.3.2/doc/readme)
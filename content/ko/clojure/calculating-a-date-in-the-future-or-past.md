---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Clojure: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

미래 또는 과거의 날짜 계산은 특정 기간 이후 또는 이전의 날짜를 찾는 작업입니다. 프로그래머들은 이를 통해 예약, 데드라인 계산, 이벤트 스케줄링 등을 관리합니다.

## 방법:

두 날짜 사이의 차이를 계산하는 Clojure 스크립트의 예를 보겠습니다.

```clojure
(require '[clj-time.core :as t])
(require '[clj-time.period :as p])

(let [from (t/date-time 2020 12 31 12 0 0 0)
      to (t/date-time 2021 1 7 12 0 0 0)
      period (p/in-days from to)]
  (println period))
```

이 스크립트를 실행하면 `7`이 출력됩니다, 약 7일 후라는 것을 보여줍니다.

## 딥 다이브:

#### 역사적 배경:

클로저는 Lisp에 기반한 함수형 프로그래밍 언어입니다. 이 언어에서 날짜와 시간 계산은 긴 역사를 가지고 있습니다. clj-time 라이브러리는 이 역사를 계승하면서 JOoda-Time 라이브러리를 기반으로 만들어졌습니다.

#### 대안들:

모든 Clojure 프로그래머가 필요로 하지 않을 수 있는 기능이므로 일부 프로그래머는 Java의 Calendar 클래스를 직접 사용할 수 있습니다.

#### 구현 세부 사항:

clj-time 라이브러리는 일시를 표현하는 유연한 방법을 제공합니다. 이 라이브러리는 월, 주, 일, 시간, 분, 초 단위로 주기를 계산할 수 있습니다.

## 참고하려면:

- [clj-time 공식 문서](https://github.com/clj-time/clj-time)
- [Java Calendar 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Lisp Programming and Data Manipulation](https://books.google.co.kr/books?id=jcbZDwAAQBAJ)
---
title:                "Clojure: 미래나 과거의 날짜 계산하기"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심을 가질 수 있는 이유는 매우 다양합니다. 예를 들어, 기념일이나 이벤트를 준비하기 위해 미래의 특정 날짜를 계산하거나, 어떤 사건이 발생한 지 몇 일이 지났는지를 알기 위해서도 날짜 계산이 필요할 수 있습니다. 또한 개발자들은 프로그램에서 날짜를 다룰 때 이러한 기능이 필수적입니다.

## 사용 방법

```Clojure
(require '[clojure.java-time :as t]) 

(t/plus (t/today) (t/period 2 :days))
;; => #object[java.time.LocalDate 0x420e52b8 "2022-02-23"]

(t/minus (t/now) (t/period 1 :years))
;; => #object[java.time.LocalDateTime 0x1b5a6b57 "2020-02-23T16:23:16.21"]
```

위의 코드는 `clojure.java-time` 라이브러리를 사용하여 오늘 날짜에서 2일을 더하거나, 현재 시각에서 1년을 뺀 날짜를 계산하는 예시를 보여줍니다. `t/plus` 함수는 두 개의 인자를 받아서 첫 번째 인자(`LocalDate`나 `LocalDateTime` 객체)에 두 번째 인자인 `Period` 객체를 더해주는 함수입니다. `t/period` 함수는 시간 단위와 숫자를 입력받아서 해당 단위로 날짜를 계산할 수 있는 `Period` 객체를 반환합니다. `t/minus` 함수는 `plus`와 동일한 기능을 하지만 첫 번째 인자에 두 번째 인자를 뺀 값을 반환합니다.

## 깊이 파고들기

자세한 날짜 계산 방법을 알기 위해서는 `java.time` 패키지에 대해 더 알아야 합니다. `plus`와 `minus` 함수에서 사용한 `Period` 객체는 `t/period` 함수를 통해 만들었지만, `java.time` 패키지에서 제공하는 다른 클래스들을 사용할 수도 있습니다. 예를 들어, 여러 개의 날짜 객체를 비교하거나, 특정 날짜를 원하는 형식으로 출력하거나, 지역 시간대를 고려해야할 경우 `LocalDate`, `LocalDateTime` 외에도 `ZonedDateTime`, `OffsetDateTime` 등을 사용할 수 있습니다.

## 참고하기

* [java.time 패키지 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
* [Clojure java-time 라이브러리 문서](https://cljdoc.org/d/clojure.java-time/clojure.java-time/0.2.0/doc/readme)
* [인프런 - 클로저로 배우는 함수형 프로그래밍 기초: 날짜와 시간](https://www.inflearn.com/course/%ED%81%B4%EB%A1%9C%EC%A0%80-%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8-%EA%B8%B0%EC%B4%88-%EB%82%A0%EC%A7%9C-%EC%8B%9C%EA%B0%84/dashboard)
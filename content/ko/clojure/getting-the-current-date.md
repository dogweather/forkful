---
title:                "현재 날짜 가져오기"
html_title:           "Clojure: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

날짜와 시간은 우리 일상생활에서 굉장히 중요한 역할을 합니다. 우리는 일을 하고 스케줄을 관리하며 생일을 축하하고 공휴일을 기념하기도 합니다. 그래서 현재 날짜를 얻는 것은 아주 유용한 일입니다. Clojure는 간단한 명령어로 현재 날짜를 얻을 수 있게 해줍니다.

## 사용 방법

날짜와 시간을 다루기 위해서는 먼저 Clojure의 `java.time` 라이브러리를 불러와야 합니다. 그리고 `java.time.LocalDate` 클래스의 `now` 메소드를 사용하면 현재 날짜를 얻을 수 있습니다.

```Clojure
(require '[java.time :as time]) ; java.time 라이브러리 불러오기

(def today (time/LocalDate/now)) ; 현재 날짜를 today 변수에 저장

; today 변수 출력해보기
;; #object[java.time.LocalDate 0x69f02388 "2020-07-27"]
```

위 코드를 실행하면 `#object[java.time.LocalDate 0x69f02388 "2020-07-27"]`와 같은 결과를 얻을 수 있습니다. 이는 `java.time.LocalDate` 클래스의 인스턴스를 나타냅니다. 하지만 더 쉽게 날짜를 확인하기 위해 `format` 메소드를 사용할 수 있습니다.

```Clojure
(def formatted-date (time/format today "yyyy년 MM월 dd일"))

; formatted-date 변수 출력해보기
;; "2020년 07월 27일"
```

위 코드를 실행하면 `"2020년 07월 27일"`라는 형식으로 날짜를 확인할 수 있습니다. `format` 메소드의 첫 번째 인자에는 원하는 날짜 형식을 지정할 수 있습니다. 예를 들어 `"dd/MM/yyyy"`와 같이 입력하면 `"27/07/2020"`과 같은 형태로 날짜가 출력됩니다.

## 깊이 파고들기

Clojure의 `java.time` 라이브러리는 날짜와 시간을 다루는 다양한 클래스와 메소드를 제공합니다. 예를 들어 `year`, `month`, `dayOfMonth` 메소드를 사용하면 해당 날짜의 연도, 월, 일을 따로 얻을 수 있습니다. 또한 `parse` 메소드를 사용하면 문자열을 `LocalDate` 클래스로 변환할 수도 있습니다.

```Clojure
(def specific-date (time/LocalDate/parse "2020-12-25")) ; 문자열을 LocalDate 클래스로 변환

(time/year specific-date) ; 연도 출력
;; 2020

(time/month specific-date) ; 월 출력
;; #object[java.time.Month 0x6fe648de "DECEMBER"]

(time/month specific-date :text) ; 월의 이름 출력
;; "DECEMBER"

(time/dayOfMonth specific-date) ; 일 출력
;; 25
```

위 예시에서는 `parse` 메소드에 변환할 문자열과 함께 `YYYY-MM-dd`와 같은 형식을 지정해주어야 합니다. 또한 `month` 메소드에서는 `:text` 인자를 추가하여 월의 이름을 문자열로 얻을 수 있습니다.

## 참고 자료

- [Java 8에서 날짜와 시간 다루기](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure Docs: java.time](https://clojuredocs.org/java.time#refer-refer)
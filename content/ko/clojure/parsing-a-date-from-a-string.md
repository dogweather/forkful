---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:35:50.061152-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱한다는 것은 문자열 형태로된 날짜를 프로그램이 이해할 수 있는 날짜 객체로 변환하는 것이다. 프로그래머들은 데이터 저장, 검색, 사용자 입력 처리 시 이 과정을 활용한다.

## How to: (실행 방법)
Clojure에서 날짜를 파싱하는 가장 일반적인 방법은 `java.time` 패키지를 사용하는 것이다.

```Clojure
(require '[java-time :as jt])

;; 문자열에서 LocalDate로 파싱
(def date-str "2023-04-01")
(def parsed-date (jt/local-date date-str))
(parsed-date) ;; => #object[java.time.LocalDate 0x... "2023-04-01"]

;; 혹은 패턴을 지정하여 파싱
(def custom-date-str "01/04/2023")
(def date-pattern "dd/MM/yyyy")
(def parsed-custom-date (jt/local-date custom-date-str date-pattern))
(parsed-custom-date) ;; => #object[java.time.LocalDate 0x... "2023-04-01"]
```

파싱한 날짜를 사용하여 다양한 날짜 연산을 할 수 있다.

```Clojure
;; 날짜에 일주일 더하기
(def one-week-later (jt/plus-days parsed-date 7))
(one-week-later) ;; => #object[java.time.LocalDate 0x... "2023-04-08"]
```

## Deep Dive (깊이 들여다보기)
날짜 파싱은 오래전부터 필요한 기능이었다. Clojure가 JVM 기반 랭귀지이기 때문에, `java.util.Date`나 `java.util.Calendar`와 같은 자바 라이브러리를 활용한 방식으로 시작했다. 이후, `java.time` (Joda-Time에서 영향을 받은) 패키지가 자바 8부터 등장하면서 더욱 쉽고 안정적으로 날짜를 다룰 수 있게 되었다.

클래스 변환 없이 바로 사용하는 방식(`java.time`) 말고도, `clj-time`이라는 라이브러리를 사용하는 방법도 있다. 이 라이브러리는 Joda-Time을 기반으로 하며, Clojure에서 더 자연스러운 날짜 타임 기능을 제공한다.

장점은 사용 용이성과 Clojure에 더 잘 맞는 인터페이스를 가짐이지만, java.time에 비해 성능적인 측면에서 손실이 있을 수 있다는 점을 고려해야 한다.

## See Also (참고 자료)
- Clojure 공식 문서: [https://clojure.org/](https://clojure.org/)
- `java-time` 라이브러리: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
- `clj-time` 라이브러리: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- 자바 8 `java.time` 패키지 문서: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
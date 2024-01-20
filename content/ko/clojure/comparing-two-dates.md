---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교하는 것은 일련의 시간 단위 (연, 월, 일 등)를 비교하는 프로그래밍 작업을 말합니다. 프로그래머들이 이를 실행하는 이유는 스케줄링, 타이머, 데드라인과 같은 여러 시간기반 요소를 정확하게 처리하기 위해서입니다.

## 어떻게

다음 Clojure 코드는 두 날짜를 비교하는 방법을 보여줍니다.

```Clojure
(use 'clj-time.core)
(use 'clj-time.coerce)

(defn compare-dates [date1 date2]
    (let [d1 (to-local-date date1)
          d2 (to-local-date date2)]
     (.compareTo d1 d2)))
```

비교하려는 두 날짜를 인자로 받아서 LocalDateTime으로 변환한 후, `.compareTo` 메서드를 사용하여 두 날짜를 비교합니다.

## 깊게 보기

날짜를 비교하는 것은 오래된 컴퓨팅 문제입니다. 다양한 방식이 있지만, 이 중 표준적인 방법은 Joda-Time 라이브러리를 사용하는 것입니다. Clojure에서 Joda-Time을 사용하는 방법 중 하나는 clj-time 라이브러리를 이용하는 것입니다. 

위의 코드 예제에서 보았듯이, clj-time 라이브러리의 `to-local-date` 함수를 사용하여 문자열을 LocalDateTime 객체로 변환할 수 있습니다. 그 다음에는 Joda-Time의 `.compareTo` 메서드를 사용하여 두 날짜를 비교하게 됩니다.

## 참고 사항

다음은 이 문제에 대한 추가 정보와 기타 관련 자료를 찾을 수 있는 링크입니다:

1. Clojure clj-time 라이브러리: https://github.com/clj-time/clj-time
2. Joda-Time 라이브러리: https://www.joda.org/joda-time/
3. Java 8에서의 날짜 및 시간 처리: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
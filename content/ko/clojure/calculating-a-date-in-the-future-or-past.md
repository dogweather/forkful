---
title:    "Clojure: 나중이나 과거의 날짜를 계산하는 방법"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심을 갖을만한 이유는 있을까요? 예를 들어, 생일이나 기념일을 알고 싶거나, 당신이 가고 싶은 휴가나 여행을 계획하기 위해 언제 좋은 시간인지 알고 싶을 수 있습니다.

## 어떻게 해야 할까요?

캘린더를 사용하여 미래나 과거의 특정 날짜를 계산하는 것은 Clojure에서 매우 간단합니다. 예를 들어, 2022년 1월 1일로부터 100일이 지난 날짜를 계산하려면 다음과 같이 작성할 수 있습니다:

```Clojure
(calendar/date-time 2022 1 1)  ; 2022년 1월 1일을 나타내는 날짜
(calendar/plus (calendar/date-time 2022 1 1) :days 100)  ; 100일이 추가된 날짜 출력
```

결과는 2022년 4월 11일이 될 것입니다.

만약 미래나 과거의 특정 날짜에서 일정 기간을 빼고 싶다면, `-` 연산자를 사용할 수 있습니다. 예를 들어, 다음 코드는 오늘 날짜로부터 1년 전의 날짜를 계산합니다:

```Clojure
(calendar/date-time)  ; 오늘 날짜
(calendar/minus (calendar/date-time) :years 1)  ; 1년 전 날짜 출력
```

현재 날짜는 매번 실행할 때마다 다르기 때문에, 결과도 달라질 수 있습니다. 하지만 위 코드는 현재 날짜 대신 아무 날짜나 사용하셔도 무방합니다.

## 딥 다이브

Clojure에서 `calendar` 라이브러리를 사용하여 날짜를 계산하는 방법은 다양합니다. `plus`와 `minus` 외에도 `range`, `interval`, `end-of`, `day-of-week`, `with-timezone` 등의 함수를 사용할 수 있습니다. 또한 표준 라이브러리인 `java.time`과 `java.time.temporal`을 사용하여 날짜 계산을 할 수도 있습니다. 더 깊이 알아보려면 Clojure 공식 문서를 참조하세요!

## 참고

- [Clojure 공식 문서: date and time](https://clojure.org/reference/java_interop#date_and_time)
- [java.time 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [java.time.temporal 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/temporal/package-summary.html)
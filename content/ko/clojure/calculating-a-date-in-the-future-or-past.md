---
title:                "미래나 과거에서 날짜를 계산하는 방법"
html_title:           "Clojure: 미래나 과거에서 날짜를 계산하는 방법"
simple_title:         "미래나 과거에서 날짜를 계산하는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

날짜를 미래나 과거로 계산하는 것은 프로그래머들이 자주 하는 작업입니다. 이를 통해 날짜와 시간과 관련된 복잡한 로직을 쉽게 다룰 수 있습니다. 예를 들어, 내일 무슨 일이 발생할지를 예측하는 알고리즘 등이 있습니다.

# 방법:

```Clojure
(require '[clojure.java-time :as t])

(def now (t/instant))
(def one-day (t/days 1))
(def next-day (t/plus now one-day))

(t/local-date next-day)
```

출력:
```
2021-10-25
```

이 코드는 현재 날짜와 시간을 가져와서 하루를 더한 새로운 날짜를 계산하는 예제입니다. Clojure의 java-time 라이브러리를 사용하여 미래 날짜를 계산하고 출력하는 간단한 방법입니다.

# 더 알아보기:

## 역사적 배경:

날짜와 시간을 계산하는 작업은 컴퓨터 프로그래밍의 초기부터 중요한 부분입니다. 예를 들어, 프로그래머들은 과거의 데이터를 분석하거나 미래의 이벤트를 예측하기 위해 날짜와 시간을 계산해야 했습니다. 이를 위해 수많은 알고리즘과 라이브러리가 개발되었고, Clojure의 java-time 라이브러리는 그 중 하나입니다.

## 대안:

Clojure의 java-time 라이브러리 외에도 날짜와 시간을 계산하는 다양한 방법이 있습니다. 예를 들어, Java에서 제공하는 java.util 패키지를 활용할 수도 있습니다. 또는 moment.js와 같은 JavaScript 라이브러리를 사용하여 날짜와 시간을 다룰 수도 있습니다.

## 구현 세부사항:

Clojure의 java-time 라이브러리는 자바의 java.time 패키지를 Clojure의 함수로 다룰 수 있도록 감싸줍니다. 실제로 날짜와 시간을 계산하는 작업은 이 패키지에서 이루어집니다. java.time 패키지의 다양한 클래스와 메서드를 사용하여 원하는 기능을 수행할 수 있습니다.

# 더 찾아보기:

- Clojure java-time 라이브러리: https://github.com/dm3/clojure.java-time
- Java의 java.time 패키지: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- moment.js: https://momentjs.com/
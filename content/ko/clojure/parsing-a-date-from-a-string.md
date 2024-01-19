---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
문자열에서 날짜를 파싱한다는 것은 문자열 형태로 되어있는 날짜를 컴퓨터가 이해할 수 있는 형태로 변환하는 것을 의미합니다. 프로그래머들은 데이터를 분석하거나 처리할 때 이런 변환이 필요하며, 이를 위해 Clojure에서는 java.time 패키지를 사용할 수 있습니다.

## 사용 방법:
Clojure에서 문자열로부터 날짜를 파싱하는 방법을 간략히 살펴보겠습니다.

```Clojure
(require '[clojure.java-time :as jt])

(defn string-to-date [s]
  (jt/parse s))

(string-to-date "2021-12-31")
```

이렇게 하면 결과는 "2021-12-31" 문자열이었던 날짜가 java.time.LocalDate 형태로 변환되는 것을 확인할 수 있습니다.

## 깊이 있게 알아보기:
이 방식의 역사적 맥락, 대안, 그리고 구현에 대한 세부적인 정보를 살펴보겠습니다.

1. 역사적 맥락: 문자열에서 날짜를 파싱하는 작업은 가장 초기의 프로그래밍부터 필요했던 작업입니다. 복잡한 시간 포맷을 다루기 위해 여러 표준과 라이브러리가 등장하였고, 그 중에서도 java.time 패키지는 현재 가장 널리 쓰이고 있습니다.
2. 대안: java.time 패키지 뿐 아니라, Joda-Time나 Apache Commons Lang 등 다른 라이브러리들도 이용할 수 있습니다. 하지만 대부분의 경우 java.time 패키지가 충분합니다.
3. 구현 세부정보: 위의 예제에서는 단순한 "YYYY-MM-DD" 포맷에 대해서만 언급하였습니다. 하지만 java.time 패키지는 서로 다른 형태의 문자열에서도 날짜를 파싱하는 기능을 제공하며, 이는 java.time.format.DateTimeFormatter 클래스를 통해 구현됩니다.

## 관련 자료:
아래에는 문자열에서 날짜를 파싱하고 이를 이용한 프로그래밍에 대한 추가적인 자료가 있습니다.

1. Official Clojure Documentation (https://clojure.org)
2. Java.time Tutorial (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. Clojure Cookbook (https://clojure-cookbook.com)
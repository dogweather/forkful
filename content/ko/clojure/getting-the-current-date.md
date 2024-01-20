---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 알아내는 것은, 시스템 세계에서 현재 시점을 파악하기 위해 프로그램 내에 이 시점에 맞춘 작업을 수행하는 경우에 필요합니다. 프로그래머들이 이를 구현하는 이유는 다양한 날짜/시간 기반의 함수, 로그, 통계 등에 이를 사용하기 위해서입니다.

## 어떻게:

Clojure는 현재 날짜를 얻기 위해 java.util.Date의 인스턴스를 생성하는 함수 `current-date`를 제공합니다.

```Clojure
(import 'java.util.Date)
(defn current-date []
  (Date.))
```

그리고 이 함수를 호출하여 현재 날짜를 얻을 수 있습니다:

```Clojure
(current-date)
```

이 코드의 출력은 현재 날짜와 시간에 대한 정보를 포함하는 java.util.Date 객체입니다.

## 딥다이브하기:

Clojure가 탄생한 이래로, 현재 날짜와 시간의 표현 방식은 여러 사람들에게 익숙한 java.util.Date 및 java.util.Calendar 객체를 참조하여 견고하게 유지되었습니다. 그러나 Java 8에서는 java.time 패키지를 도입하여 (알려진) 날짜 및 시간 관련 문제를 개선하고, 이들이 가진 가변성, 불명확성, 그리고 비효율성을 줄였습니다. Clojure 커뮤니티도 이에 착수하여 java.time 패키지의 API를 이용하는 일련의 라이브러리를 만들었습니다. 이것이 clj-time와 java-time 그리고 다른 것들입니다.

## 참고하기:

관련 참고 링크는 다음과 같습니다:
1. Clojure 공식 웹사이트 - [Clojure.org](https://clojure.org/)
2. Java의 java.util.Date API 문서 - [Java 8 API](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
3. Java의 java.time 패키지 API 문서 - [Java 8 API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
4. clj-time GitHub 소스코드 - [clj-time](https://github.com/clj-time/clj-time)
5. java-time GitHub 소스코드 - [java-time](https://github.com/dm3/clojure.java-time)
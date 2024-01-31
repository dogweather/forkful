---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:14:04.720398-07:00
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 얻는 것은 시스템의 현재 날짜와 시간 정보를 추출하는 과정입니다. 개발자들은 로깅, 타임스탬프, 기능의 스케줄링 등 목적으로 이 정보를 활용합니다.

## How to (어떻게):
Clojure에서 현재 날짜를 얻는 것은 `java.util.Date` 클래스와 함께 Clojure의 자체 함수들을 사용하는 것을 포함합니다. 짧고 간단한 예제를 살펴봅시다:

```clojure
;; java.util.Date 사용
(import '[java.util Date])
(str (Date.))

;; clj-time 라이브러리 사용 (Joda-Time 래퍼)
(require '[clj-time.core :as t])
(str (t/now))

;; java-time 라이브러리 사용 (Java 8 Date and Time API 래퍼)
(require '[java-time :as jt])
(str (jt/local-date))
```

실행 결과는 현재 시스템의 날짜와 시간을 반영한 문자열입니다:

```
"Tue Mar 07 14:58:17 KST 2023"
"2023-03-07T14:58:17.123Z"
"2023-03-07"
```

## Deep Dive (심층 분석):
Clojure는 자바 가상 머신(JVM) 위에서 실행되기 때문에, 자바의 날짜와 시간 관련 클래스에 쉽게 접근할 수 있습니다. `java.util.Date`는 오래되었지만, 여전히 사용됩니다. `clj-time` 라이브러리는 Joda-Time을 래핑하며, 좀 더 Clojure스러운 API를 제공합니다. 그리고 `java-time` 라이브러리는 Java 8에서 새롭게 추가된 Date and Time API를 감싸고 있어, 더욱 현대적이고, 불변의(immutable) 장점을 가지고 있습니다.

과거에는 자바의 `java.util.Date`와 `java.util.Calendar`가 주로 사용되었지만, 스레드-세이프하지 않고, 설계상 문제점을 가지고 있어 Java 8부터는 새로운 `java.time` 패키지가 사용을 권장되고 있습니다. Clojure는 이러한 자바의 진화를 잘 반영하여, 이를 활용하는 다양한 라이브러리와 방법을 제공합니다. 

Clojure에서 날짜를 다루는 것은 자바와 매우 유사하지만, 함수형 프로그래밍 패러다임에 맞게 불변성(Immutability)과 사이드 이펙트를 최소화하는 방향으로 사용하는 것이 주된 차이점입니다.

## See Also (참고자료):
- [Clojure 공식 문서](https://clojure.org/)
- [clj-time GitHub 페이지](https://github.com/clj-time/clj-time)
- [java-time GitHub 페이지](https://github.com/dm3/clojure.java-time)
- [Java 8 Date and Time 관련 공식 튜토리얼](https://docs.oracle.com/javase/tutorial/datetime/)

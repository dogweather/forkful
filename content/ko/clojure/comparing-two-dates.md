---
title:                "두 날짜 비교하기"
html_title:           "Clojure: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
이 글에서는 Clojure를 사용하여 두 날짜를 비교하는 방법을 배우게 됩니다. 날짜 비교는 두 날짜가 이전 날짜인지 이후 날짜인지 확인하는 것입니다. 프로그래머들은 날짜 비교를 사용하여 다양한 목적으로 데이터를 정렬하거나 필터링하는 데 사용합니다.

## 방법:
```Clojure
(def date1 (java.util.Date. 120, 1, 2))
(def date2 (java.util.Date. 120, 1, 1))

(if (.after date1 date2)
  (println "date1은 date2보다 이후 날짜입니다.")
  (println "date1은 date2보다 이전 날짜입니다."))
```
출력: `date1은 date2보다 이후 날짜입니다.`

## 깊게 파고들기:
- 일부 프로그래밍 언어에서는 날짜와 시간을 비교할 때 문제가 발생했습니다. Clojure는 이러한 문제를 해결하기 위해 `java.util.Date` 클래스를 사용합니다.
- 날짜 비교에는 `java.util.Date.after()` 메서드를 사용합니다.
- 다른 예제로는 `clojure.java-time` 라이브러리와 `clj-time` 라이브러리를 사용하여 날짜 비교를 할 수도 있습니다.

## 관련 정보:
- Clojure 공식 문서: https://clojure.org/reference/java_interop
- `clojure.java-time` 라이브러리: https://github.com/dm3/clojure.java-time
- `clj-time` 라이브러리: https://github.com/clj-time/clj-time
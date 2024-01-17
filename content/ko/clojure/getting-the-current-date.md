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

## 무엇이며 그 이유는? 
현재 날짜를 가져오는 것은 프로그래머가 많이 사용하는 작업입니다. 이것은 현재 시간을 확인하고 현재 사용을 식별하는데 도움이 됩니다.

## 하는 법: 
```Clojure
; 현재 날짜 가져오기
(def today (java.util.Date.))

; 날짜 포맷 지정 (예제: 2019년 1월 1일)
; MMMM은 월의 이름을 나타내고, dd는 일을 나타냅니다.
(java.time.format.DateTimeFormatter.ofPattern "MMMM dd, yyyy").format(today)
;; => "January 01, 2019"

; 현재 시간 가져오기
; HH는 24시간 형식으로 시간을 나타냅니다.
(java.time.format.DateTimeFormatter.ofPattern "HH:mm:ss").format(today)
;; => "10:21:54"

```

## 딥 다이브: 
현재 날짜를 가져오는 작업은 Java 표준 라이브러리인 java.util.Date를 통해 가능합니다. Clojure는 Java 언어의 이점을 살려 이를 접근할 수 있습니다. 또한 java.time 패키지를 통해 더 많은 날짜 및 시간 관련 기능을 사용할 수 있습니다. 다른 대안으로는 clj-time이 있는데, 이는 Clojure에서 날짜 및 시간을 다루기 쉽게 도와줍니다. 이러한 라이브러리들을 사용하면 더 많은 기능을 활용할 수 있습니다.

## 관련 정보: 
- [Clojure docs: java.util.Date](https://clojure.org/reference/java_interop#java_util_Date)
- [Oracle docs: java.time 패키지](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time 라이브러리](https://github.com/clj-time/clj-time)
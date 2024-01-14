---
title:    "Clojure: 현재 날짜 받아오기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜
현재 날짜를 얻는 것을 왜 해야할까요?

현재 날짜를 얻는 것은 우리가 다양한 프로그램에서 사용하는 중요한 기능입니다. 예를 들면, 만료일 계산, 날짜별 이벤트 제어, 그리고 보고서 작성 등 다양한 목적에 사용될 수 있습니다. 따라서 현재 날짜를 얻는 것은 매우 유용한 프로그래밍 기술입니다.

# 어떻게
```Clojure
(require '[clojure.java-time :as time])

(time/local-date)
;=> #object[java.time.LocalDate 0x2b86173c "2020-09-20"]

(time/local-date-time)
;=> #object[java.time.LocalDateTime 0x2f4233f2 "2020-09-20T10:34:54.620"]

(time/current-time)
;=> #object[java.time.LocalTime 0x42b49c13 "10:34:54.620"]
```

Clojure에서 현재 날짜를 얻는 가장 쉬운 방법은 `clojure.java-time` 라이브러리를 통해 `local-date`, `local-date-time`, `current-time` 함수를 사용하는 것입니다. 이러한 함수는 각각 현재 날짜, 현재 날짜와 시간, 현재 시간 객체를 반환하여 우리가 다양한 목적에 활용할 수 있도록 합니다.

# 깊게 파고들기
Clojure에서 현재 날짜를 가져오는 함수들은 Java의 `java.time` 라이브러리를 기반으로 작성되었습니다. 이 라이브러리는 날짜와 시간을 다루는 다양한 기능을 제공하며, Clojure에서도 자유롭게 사용할 수 있습니다. `clojure.java-time` 라이브러리의 많은 기능을 자세히 알고 싶다면 [공식 문서](https://cljdoc.org/d/java-time/java-time/1.0.1/doc/readme)를 살펴보세요.

# 관련 링크
- [Java `java.time` 라이브러리](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure `clojure.java-time` 라이브러리 공식 문서](https://cljdoc.org/d/java-time/java-time/1.0.1/doc/readme)
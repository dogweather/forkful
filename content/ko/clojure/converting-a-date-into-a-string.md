---
title:    "Clojure: 날짜를 문자열로 변환하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜: 날짜를 문자열로 변환하는 것에 참여하는 이유는?

날짜를 문자열로 변환하는 작업은 자주 필요한 일입니다. 예를 들어, 데이터베이스에 저장된 날짜를 사용자가 이해할 수 있는 형식으로 표시하거나, 다양한 시스템 간에 날짜를 교환하는 경우에 필요합니다.

## 어떻게 하나요?

```clojure
(require '[clojure.java.time :as t])

;; 오늘 날짜를 문자열로 변환하기
(t/local-date->str (t/today))

;; 원하는 날짜 형식으로 변환하기
(t/format (t/date-time 2021 9 1) "MM/dd/yy")

;; 시간대 및 로케일 설정하기
(t/local-date->str (t/today) {:zone "UTC" :locale "en-US"})
```

위 예제에서는 `clojure.java.time` 네임스페이스에서 제공하는 유틸리티 함수를 사용하여 날짜를 문자열로 변환하는 방법을 보여줍니다. `local-date->str` 함수는 `LocalDate` 객체를 문자열로 변환할 수 있으며, `format` 함수는 원하는 날짜 형식으로 변환할 수 있습니다. 옵션으로는 시간대와 로케일을 설정할 수도 있습니다.

## 깊이있게 살펴보기

Clojure에서 날짜를 다루는 API는 자바 표준 라이브러리에 기반하여 만들어졌습니다. 따라서 자바의 `java.time` 패키지와 유사한 인터페이스를 가지고 있습니다. 이를 통해 간편하게 날짜를 다룰 수 있고, `java.time` 라이브러리에서 제공하는 더 많은 기능들을 활용할 수 있습니다.

Clojure에서는 `java.time`의 `LocalDate`와 `DateTimeFormatter` 클래스를 이용하여 날짜를 다룹니다. `LocalDate`는 시간이나 시간대를 포함하지 않는 날짜 정보를 나타내며, `DateTimeFormatter`는 날짜를 원하는 형식으로 표현할 수 있도록 지원합니다.

## 관련 자료

- [clojure.java.time 문서](https://clojure.github.io/java-time/)
- [자바 `java.time` 패키지 문서](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)

# 관련 자료

- [clojure.java.time 문서](https://clojure.github.io/java-time/)
- [자바 `java.time` 패키지 문서](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
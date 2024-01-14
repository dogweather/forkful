---
title:                "Clojure: 날짜를 문자열로 변환하기"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜

날짜를 문자열로 바꾸는 작업을 하는 이유는 무엇일까요? 대부분의 프로그램에서 날짜는 중요한 정보 중 하나입니다. 따라서, 날짜를 쉽게 이해하고 다룰 수 있는 형식으로 변환하는 것은 중요합니다. 또한, 날짜를 문자열로 바꾸면 다양한 포맷으로 표현할 수 있기 때문에 유용합니다.

# 어떻게

Clojure에서 날짜를 문자열로 바꾸는 방법은 다양한 함수를 사용할 수 있습니다. 예를 들어, `clj-time` 라이브러리를 사용하면 날짜를 포맷팅하는데 유용한 `format` 함수를 사용할 수 있습니다.

```Clojure
(require '[clj-time.format :as fmt])

(def today (clj-time.core/today))

(fmt/format today "MM/dd/yyyy")
;; "11/18/2020"

(fmt/format today "dd.MM.yyyy")
;; "18.11.2020"
```

또는 `java-time` 라이브러리를 사용해도 같은 결과를 얻을 수 있습니다.

```Clojure
(require '[java-time :as jt])

(def today (jt/now))

(jt/format today "MM/dd/yyyy")
;; "11/18/2020"

(jt/format today "dd.MM.yyyy")
;; "18.11.2020"
```

위의 예시에서는 `MM`은 월을, `dd`는 일을, `yyyy`는 년도를 나타냅니다. 다른 포맷을 사용하고 싶다면 Java의 날짜 포매팅 규칙을 참고하면 됩니다. 또한, `locale` 매개변수를 사용하여 다른 언어로 표현할 수도 있습니다.

# 더 깊이 파고들기

날짜를 문자열로 바꾸는 과정은 다소 복잡합니다. 날짜의 포맷에는 년, 월, 일, 요일, 시간 등 다양한요소가 포함될 수 있기 때문입니다. 또한, 다양한 국가 및 지역에서 사용되는 다른 날짜 형식을 처리하는 것도 중요합니다. 따라서, `clj-time`이나 `java-time`과 같은 날짜 라이브러리를 사용하는 것이 좋습니다. 이러한 라이브러리들은 모든 세부 요소를 고려하여 일관된 방식으로 날짜를 문자열로 바꿔줄 것입니다.

# 관련 자료

- [clj-time 공식 문서](https://clj-time.github.io/clj-time/)
- [java-time 라이브러리 GitHub 페이지](https://github.com/dm3/clojure.java-time)
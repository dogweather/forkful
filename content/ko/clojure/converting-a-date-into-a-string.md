---
title:                "날짜를 문자열로 변환하는 방법"
html_title:           "Clojure: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

Clojure는 Date와 String 사이의 변환 기능을 제공하기 때문에 더 효율적으로 날짜를 다룰 수 있습니다. 따라서 Clojure를 사용하면 날짜와 관련된 작업을 더 쉽고 간편하게 수행할 수 있습니다.

## 방법

Clojure에서 Date를 String으로 변환하는 방법은 다음의 예시 코드를 통해 알려드리겠습니다. 

```Clojure 
(require '[clojure.java-time :as t])

;; 현재 날짜와 시간을 얻어옵니다.
(def current-date-time (t/local-date-time))

;; 날짜를 String으로 변환합니다.
(t/format current-date-time "yyyy-MM-dd")

;; 결과: "2021-01-01"
```

만약 포맷을 변경하고 싶다면 다음과 같이 포맷 문자열을 수정하면 됩니다.

```Clojure
;; 날짜를 "MM/dd/yyyy" 포맷으로 변환합니다.
(t/format current-date-time "MM/dd/yyyy")

;; 결과: "01/01/2021"
```

## 깊이 있는 탐구

Clojure에서 Date와 String 사이의 변환은 Java의 java.time 패키지에 있는 클래스들을 사용하여 이루어집니다. Clojure의 t/format 함수는 java.time 패키지의 DateTimeFormatter 클래스를 이용하여 날짜를 포맷팅합니다. 그리고 날짜와 시간을 다루는 데 있어서 Clojure의 java-time 라이브러리는 매우 유용한 도구입니다. 따라서 Clojure에서 날짜와 관련된 작업을 수행할 때는 java-time 라이브러리를 적극적으로 활용하는 것이 좋습니다.

## 확인해보세요
[자바타임(JT라이브)](https://docs.oracle.com/javase/8/docs/technotes/guides/i18n/formatting_l10n.html)
[Clojure java-time 라이브러리 문서](https://clojure.github.io/java-time/)
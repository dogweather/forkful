---
title:                "Clojure: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

홀리데이 계획설양을 위해 두 날짜를 비교하는 일이 자주 있습니다. Clojure를 사용하면 이 작업을 간단하게 처리할 수 있습니다.

## 어떻게

먼저 `clj-time` 라이브러리를 `project.clj` 파일의 `:dependencies` 부분에 추가해주세요.

```Clojure
:dependencies [[clj-time "0.15.2"]]
```

그리고나서 `clj-time`의 `time` 네임스페이스를 추가해줍니다.

```Clojure
(ns your-namespace
  (:require [clj-time.core :as time]))
```

이제 두 날짜를 생성하고 비교하는 예제를 살펴봅시다.

```Clojure
(def date1 (time/date 2021 1 1))
(def date2 (time/date 2021 2 1))

(println (time/compare date1 date2))
;; 결과: -1 (-1은 date1이 date2보다 이전임을 나타냅니다.)
```

또 다른 예제로는 두 날짜가 같은지 비교하는 방법입니다.

```Clojure
(def date1 (time/date 2020 12 31))
(def date2 (time/date 2021 1 1))

(println (time/same? date1 date2))
;; 결과: false (두 날짜가 다름을 나타냅니다.)
```

이렇게 간단하게 두 날짜를 비교할 수 있습니다!

## 깊이 파헤치기

`clj-time` 라이브러리는 Joda-Time의 Clojure 포팅판입니다. 따라서 Joda-Time의 모든 기능을 활용할 수 있습니다. 또한 `clj-time`은 Clojure의 함수형 프로그래밍 스타일을 지키기 때문에 이전날짜/다음날짜를 구하는 등의 기능도 쉽게 사용할 수 있습니다. 자세한 내용은 [공식 문서](https://github.com/clj-time/clj-time)를 참조하세요.

## 참고자료

- [Clojure 공식 사이트](https://clojure.org/)
- [Joda-Time 공식 사이트](https://www.joda.org/joda-time/)
- [Clj-time 라이브러리 문서](https://github.com/clj-time/clj-time)
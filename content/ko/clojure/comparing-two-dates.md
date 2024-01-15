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

## 왜

어떤 이가 두 날짜를 비교하는 것에 시간을 쏟을까요? 두 날짜 사이의 차이를 계산하고 날짜의 순서를 알아내는 경우가 있을 수 있습니다.

## 어떻게

```Clojure
(require '[clj-time.core :as t])

(def date1 (t/date 2021 1 1))
(def date2 (t/date 2021 1 15))

(t/days-between date1 date2)
```

Output: 14

```Clojure
(t/compare-dates date2 date1)
```

Output: 1

## 깊이 파헤치기

이 두 함수는 `clj-time` 라이브러리에서 제공됩니다. `days-between` 함수는 두 날짜 사이의 일 수를 계산하고 `compare-dates` 함수는 첫 번째 날짜가 두 번째 날짜보다 이전인지, 같은 날인지, 아니면 더 늦은지를 알려줍니다. 두 날짜 모두 `date-time` 데이터 형식이어야 합니다.

## 더 찾아보기

### Clojure 날짜 라이브러리
- https://github.com/clj-time/clj-time
- https://github.com/clj-commons/clj-time.format
### Clojure 날짜 비교 관련 함수
- https://clojuredocs.org/clojure.core/compare
- https://clojuredocs.org/clojure.core/<
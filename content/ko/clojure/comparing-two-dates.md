---
title:    "Clojure: 두 날짜 비교하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 것에 관심이 생길 수 있습니다. 이는 프로젝트에서 날짜 데이터를 다뤄야 할 때 또는 개인적으로 날짜를 비교하는 데 필요할 때 유용합니다.

## 어떻게

두 날짜를 비교하려면 `>` (초과), `<` (미만), `=` (같음) 연산자를 사용하거나 `clojure.core/different?` 함수를 이용할 수 있습니다. 아래에 예제 코드와 결과를 제시하겠습니다.

```Clojure
(require '[clojure.java-time :as t])

(def date1 (t/local-date 2021 4 5))
(def date2 (t/local-date 2021 4 8))

(> date1 date2) ; false
(< date1 date2) ; true
(= date1 date2) ; false

(t/different? date1 date2) ; true
```

위의 예제 코드에서는 먼저 `clojure.java-time` 라이브러리를 `require` 하고 두 날짜 `date1`과 `date2`를 생성합니다. 그 다음 `>` 연산자와 `<` 연산자, 그리고 `=` 연산자를 사용하여 두 날짜를 비교합니다. 또한 `clojure.core/different?` 함수를 이용하여 두 날짜가 서로 다른지 확인할 수도 있습니다.

## 깊게 파고들기

두 날짜를 비교할 때 주의해야 할 점이 있습니다. 두 날짜가 같은 날짜라도 시간 정보가 있을 경우 결과가 다를 수 있습니다. 예를 들어, `t/local-date-time` 함수를 이용하여 시간 정보를 추가한 두 날짜를 생성하고 비교해보겠습니다.

```Clojure
(require '[clojure.java-time :as t])

(def date1 (t/local-date-time 2021 4 5 9 30 0))
(def date2 (t/local-date-time 2021 4 5 12 0 0))

(= date1 date2) ; true
(< date1 date2) ; false
(> date1 date2) ; false
```

두 날짜 `date1`과 `date2`가 같은 날짜이지만 시간 정보가 다르기 때문에 `=` 연산자에서는 같게 반환됩니다. 그러나 `<` 연산자와 `>` 연산자에서는 다른 결과가 나타납니다. 이는 두 날짜의 정확한 비교를 원한다면 시간 정보가 없는 `t/local-date` 함수를 사용해야 한다는 것을 의미합니다.

따라서 두 날짜를 비교할 때는 두 날짜가 가지고 있는 정보를 고려하여 적절한 함수나 연산자를 선택하는 것이 중요합니다.

## 또 다른 정보 보기

- [`clojure.java-time` 공식 문서](https://cljdoc.org/d/clojure.java-time/clojure.java-time/0.3.2/api/index)
- [날짜 데이터 다루는 방법 더 알아보기](https://clojuredocs.org/clojure.java-time)
- [비교 연산자와 함수에 대한 자세한 설명](https://clojuredocs.org/clojure.core/>) 

# 또 보기